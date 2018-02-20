namespace rec Rezoom.GraphQL.AutoSchema
open System
open System.Reflection
open System.Collections
open System.Collections.Generic
open Rezoom
open Rezoom.GraphQL
open Rezoom.GraphQL.Execution

type TypeMapping<'cxt>() =
    static let mappers = ResizeArray<IConcreteTypeMapper>()
    static let cachedQueryTypes = Dictionary<Type, AutoSchemaQueryTypeBase<'cxt>>()
    static let cachedConcreteTypes = Dictionary<Type, IConcreteTypeMapping option>()
    static let globalMapper =
        { new IConcreteTypeMapper with
            member this.MapType(_, ty) =
                TypeMapping<'cxt>.GetConcreteMapping(ty)
        }
    static do
        mappers.Add(StandardTypeMappings.standardMappingProvider)
        mappers.Add(StandardTypeMappings.enumMappingProvider)
        mappers.Add(StandardTypeMappings.singleCaseWrapperProvider)
        mappers.Add(StandardTypeMappings.discriminatedUnionProvider)
        mappers.Add(StandardTypeMappings.arrayMappingProvider)
        mappers.Add(StandardTypeMappings.listMappingProvider)
        mappers.Add(StandardTypeMappings.nullableMappingProvider)
        mappers.Add(StandardTypeMappings.optionMappingProvider)
        mappers.Add(StandardTypeMappings.enumerableMappingProvider)

    static member internal QueryTypes = cachedQueryTypes
    static member internal ConcreteTypes = cachedConcreteTypes

    static member RegisterMapper(mapper : IConcreteTypeMapper) =
        mappers.Add(mapper)

    static member internal CreateConcreteTypeMapping(ty : Type) : IConcreteTypeMapping option =
        let mutable i = 0
        let mutable found = None
        while i < mappers.Count do
            match mappers.[i].MapType(globalMapper, ty) with
            | Some _ as it ->
                found <- it
                i <- Int32.MaxValue
            | None ->
                i <- i + 1
        found
            
    static member internal GetConcreteMapping(ty : Type) =
        let succ, found = cachedConcreteTypes.TryGetValue(ty)
        if succ then
            found
        else
            let it = TypeMapping<'cxt>.CreateConcreteTypeMapping(ty)
            cachedConcreteTypes.[ty] <- it
            it

    static member GetQueryableMapping(clrType : Type) =
        let succ, found = cachedQueryTypes.TryGetValue(clrType)
        if succ then found else
        cachedQueryTypes.[clrType] <- WrappedQueryableMapping<'cxt>(clrType)
        let generatedMapping = AutoSchemaQueryType<'cxt>(clrType)
        cachedQueryTypes.[clrType] <- generatedMapping
        generatedMapping :> AutoSchemaQueryTypeBase<'cxt>

type WrappedQueryableMapping<'cxt>(clrType : Type) =
    inherit AutoSchemaQueryTypeBase<'cxt>()
    let mapping = lazy TypeMapping<'cxt>.GetQueryableMapping(clrType)
    override this.Select(instance, context, fieldName, args, directives) =
        mapping.Value.Select(instance, context, fieldName, args, directives)
    override this.TypeName = mapping.Value.TypeName
    override this.Description = mapping.Value.Description
    override this.Fields = mapping.Value.Fields

type GenericBuildingBlocks =
    static member WrapSelectableOptional<'cxt, 'sel when 'sel :> ISelectable<'cxt>> (p : 'sel option)
        : FieldResolution<'cxt> Plan =
        plan {
            match p with
            | None -> return ResolvedConcrete NullValue
            | Some v -> return ResolvedSelectable v
        }
    static member WrapManySelectablesOptional<'cxt, 'sel, 'sq when 'sel :> ISelectable<'cxt> and 'sq :> 'sel seq> (p : 'sq option)
        : FieldResolution<'cxt> Plan =
        plan {
            match p with
            | None -> return ResolvedConcrete NullValue
            | Some v ->
                let result = v :> 'sel seq
                // this next part would be covariant upcast in C#... too bad we have to use Seq.cast in F#
                let result = Seq.cast<ISelectable<'cxt>> result 
                return ResolvedSelectables result
        }
    static member WrapSelectable<'cxt, 'sel when 'sel :> ISelectable<'cxt>> (p : 'sel)
        : FieldResolution<'cxt> Plan =
        plan {
            return ResolvedSelectable p
        }
    static member WrapManySelectables<'cxt, 'sel, 'sq when 'sel :> ISelectable<'cxt> and 'sq :> 'sel seq> (p : 'sq)
        : FieldResolution<'cxt> Plan =
        plan {
            let result = p :> 'sel seq
            // this next part would be covariant upcast in C#... too bad we have to use Seq.cast in F#
            let result = Seq.cast<ISelectable<'cxt>> result 
            return ResolvedSelectables result
        }
    static member WrapConcrete<'cxt, 'prim>(p : 'prim)
        : FieldResolution<'cxt> Plan =
        plan {
            let value = TypeMapping<'cxt>.GetConcreteMapping(typeof<'prim>).Value.ObjToConcrete(p)
            return ResolvedConcrete value
        }

    // Plan versions
    static member WrapPlanOfSelectableOptional<'cxt, 'sel when 'sel :> ISelectable<'cxt>>
        (p : 'sel option Plan)
        : FieldResolution<'cxt> Plan =
        Plan.bind p GenericBuildingBlocks.WrapSelectableOptional
    static member WrapPlanOfManySelectablesOptional<'cxt, 'sel, 'sq when 'sel :> ISelectable<'cxt> and 'sq :> 'sel seq>
        (p : 'sq option Plan)
        : FieldResolution<'cxt> Plan =
        Plan.bind p GenericBuildingBlocks.WrapManySelectablesOptional
    static member WrapPlanOfSelectable<'cxt, 'sel when 'sel :> ISelectable<'cxt>> (p : 'sel Plan)
        : FieldResolution<'cxt> Plan =
        Plan.bind p GenericBuildingBlocks.WrapSelectable
    static member WrapPlanOfManySelectables<'cxt, 'sel, 'sq when 'sel :> ISelectable<'cxt> and 'sq :> 'sel seq> (p : 'sq Plan)
        : FieldResolution<'cxt> Plan =
        Plan.bind p GenericBuildingBlocks.WrapManySelectables
    static member WrapPlanOfConcrete<'cxt, 'prim>(p : 'prim Plan)
        : FieldResolution<'cxt> Plan =
        Plan.bind p GenericBuildingBlocks.WrapConcrete

type internal ReturnSelectableCase =
    | ReturnsSelectableOne of selType : Type
    | ReturnsSelectableSequence of sequenceType : Type * selType : Type
    static member private TryFind<'cxt>(ty : Type, inOptional : bool) =
        if not inOptional && ty.IsGenericType && obj.ReferenceEquals(typedefof<_ option>, ty.GetGenericTypeDefinition()) then
            ReturnSelectableCase.TryFind(ty.GetGenericArguments().[0], true)
        else
            let selectable = typeof<ISelectable<'cxt>>
            match sequenceElementType ty with
            | Some (seqType, elementType) when selectable.IsAssignableFrom(elementType) ->
                Some (inOptional, ReturnsSelectableSequence (seqType, elementType))
            | None when selectable.IsAssignableFrom(ty) ->
                Some (inOptional, ReturnsSelectableOne ty)
            | _ -> None
    static member TryFind<'cxt>(ty : Type) = ReturnSelectableCase.TryFind<'cxt>(ty, false)

type internal ReturnTypeCase =
    | ReturnsConcrete of Type * IConcreteTypeMapping
    | ReturnsSelectable of optional : bool * case : ReturnSelectableCase
    static member Find<'cxt>(ty : Type) =
        match TypeMapping<'cxt>.GetConcreteMapping(ty) with
        | Some mapping -> ReturnsConcrete (ty, mapping)
        | None ->
            match ReturnSelectableCase.TryFind<'cxt>(ty) with
            | Some (optional, case) -> ReturnsSelectable(optional, case)
            | None -> failwithf "Unsupported type in schema: %O" ty

type internal ReturnType<'cxt> =
    {   Case : ReturnTypeCase
        IsWrappedInPlan : bool
    }
    static member OfType(ty : Type) =
        let isWrappedInPlan, ty =
            if ty.IsGenericType && obj.ReferenceEquals(ty.GetGenericTypeDefinition(), typedefof<_ Plan>) then
                true, ty.GetGenericArguments().[0]
            else
                false, ty
        let retCase = ReturnTypeCase.Find<'cxt>(ty)
        {   IsWrappedInPlan = isWrappedInPlan
            Case = retCase
        } : ReturnType<'cxt>
    member this.WrapperMethodInfo =
        let gbb = typeof<GenericBuildingBlocks>
        let prefix = if this.IsWrappedInPlan then "WrapPlanOf" else "Wrap"
        match this.Case with
        | ReturnsConcrete (p, _) ->
            gbb.GetMethod(prefix + "Concrete").MakeGenericMethod(typeof<'cxt>, p)
        | ReturnsSelectable (optional, ReturnsSelectableOne selType) ->
            gbb
                .GetMethod(prefix + "Selectable" + if optional then "Optional" else "")
                .MakeGenericMethod(typeof<'cxt>, selType)
        | ReturnsSelectable (optional, ReturnsSelectableSequence (seqType, selType)) ->
            gbb
                .GetMethod(prefix + "ManySelectables" + if optional then "Optional" else "")
                .MakeGenericMethod(typeof<'cxt>, selType, seqType)

[<AbstractClass>]
type AutoSchemaField<'cxt>(name : string, descr : string option, deprecReason : string option, returnType : Type) =
    let returnType = ReturnType<'cxt>.OfType(returnType)
    let fieldType =
        match returnType.Case with
        | ReturnsConcrete (_, mapping) -> ConcreteField mapping.ConcreteType
        | ReturnsSelectable (optional, ReturnsSelectableOne selTy) ->
            let queryable = TypeMapping<'cxt>.GetQueryableMapping(selTy) :> ISchemaQueryType
            QueryField { IsOptional = optional; IsList = false; QueryType = queryable }
        | ReturnsSelectable (optional, ReturnsSelectableSequence (_, selTy)) ->
            let queryable = TypeMapping<'cxt>.GetQueryableMapping(selTy) :> ISchemaQueryType 
            QueryField { IsOptional = optional; IsList = true; QueryType = queryable }
    member internal this.ReturnType = returnType
    member this.FieldName = name
    member this.Description = descr
    member this.DeprecatedForReason = deprecReason
    member this.FieldType = fieldType
    abstract Selector
        : instance : obj
        * context : 'cxt
        * args : ExecArgument ListWithSource
        * directives : ExecDirective ListWithSource
        -> FieldResolution<'cxt> Plan
    abstract Arguments : IReadOnlyDictionary<string, ISchemaArgument>
    default this.Arguments = upcast Dictionary()
    abstract member EstimateComplexity : ISchemaArgument seq -> Complexity
    default this.EstimateComplexity(_) = 1L
    interface ISchemaField with
        member this.FieldType = this.FieldType
        member this.FieldName = this.FieldName
        member this.Description = this.Description
        member this.DeprecatedForReason = this.DeprecatedForReason
        member this.Arguments = this.Arguments
        member this.EstimateComplexity(args) = this.EstimateComplexity(args)

type AutoSchemaMethod<'cxt>(name, descr, deprecReason, underlying : MethodInfo) as this =
    inherit AutoSchemaField<'cxt>(name, descr, deprecReason, underlying.ReturnType)
    let args = Dictionary()
    let mutable setContext = fun (_ : obj array) (_ : 'cxt) -> ()
    let setters = Dictionary()
    let parameters = underlying.GetParameters()
    let resultConverter = this.ReturnType.WrapperMethodInfo
    do
        for par in parameters do
            if obj.ReferenceEquals(par.ParameterType, typeof<'cxt>) then
                setContext <- fun argValues context -> argValues.[par.Position] <- box context
            else
                let arg =
                    let descr = GraphQLDescriptionAttribute.GetDescription(par)
                    let argType =
                        match TypeMapping<'cxt>.GetConcreteMapping(par.ParameterType) with
                        | Some mapping -> mapping
                        | None -> failwith "Queryable types cannot be argument types"
                    setters.[par.Name] <-
                        fun (argValues : obj array) (argValue : ConcreteValue) ->
                            argValues.[par.Position] <- argType.ConcreteToObj(argValue)
                    { new ISchemaArgument with
                        member this.ArgumentName = par.Name
                        member this.ArgumentType = argType.ConcreteType
                        member this.Description = descr
                    }
                args.Add(arg.ArgumentName, arg)
    new(underlying : MethodInfo) =
        AutoSchemaMethod<'cxt>
            ( GraphQLNameAttribute.GetName(underlying)
            , GraphQLDescriptionAttribute.GetDescription(underlying)
            , GraphQLDeprecationAttribute.GetDeprecatedReason(underlying)
            , underlying
            )
    override this.Arguments = upcast args
    override this.Selector(instance, context, args, _) =
        let argValues = Array.zeroCreate parameters.Length
        setContext argValues context
        for arg in args do
            let succ, setter = setters.TryGetValue(arg.Value.Argument.ArgumentName)
            if succ then
                setter argValues arg.Value.Value
        plan {
            let rawObj = underlying.Invoke(instance, argValues)
            let plan = resultConverter.Invoke(null, [| rawObj |]) :?> FieldResolution<'cxt> Plan
            return! plan
        }

type AutoSchemaProperty<'cxt>(underlying : PropertyInfo) =
    inherit AutoSchemaMethod<'cxt>
        ( GraphQLNameAttribute.GetName(underlying)
        , GraphQLDescriptionAttribute.GetDescription(underlying)
        , GraphQLDeprecationAttribute.GetDeprecatedReason(underlying)
        , underlying.GetGetMethod()
        )

[<AbstractClass>]
type AutoSchemaQueryTypeBase<'cxt>() =
    abstract Select
        : instance : obj
        * context : 'cxt
        * fieldName : string
        * args : ExecArgument ListWithSource
        * directives : ExecDirective ListWithSource
        -> FieldResolution<'cxt> Plan
    abstract TypeName : string
    abstract Description : string option
    abstract Fields : IReadOnlyDictionary<string, ISchemaField>
    interface ISchemaQueryType with
        member this.TypeName = this.TypeName
        member this.Description = this.Description
        member this.Fields = this.Fields

type AutoSchemaQueryType<'cxt>(underlying : Type) =
    inherit AutoSchemaQueryTypeBase<'cxt>()
    let name = GraphQLNameAttribute.GetName(underlying)
    let descr = GraphQLDescriptionAttribute.GetDescription(underlying)
    let includeMemberInfo (mem : MemberInfo) =
        let custom = mem.GetCustomAttribute<GraphQLIgnoreAttribute>(``inherit`` = true)
        isNull(custom)
        && not (obj.ReferenceEquals(mem.DeclaringType, typeof<obj>))
    let fields =
        seq {
            for method in underlying.GetMethods(BindingFlags.Public ||| BindingFlags.Instance) do
                if includeMemberInfo method && not method.IsSpecialName then
                    yield AutoSchemaMethod<'cxt>(method) :> _ AutoSchemaField
            for prop in underlying.GetProperties(BindingFlags.Public ||| BindingFlags.Instance) do
                if includeMemberInfo prop then
                    yield AutoSchemaProperty<'cxt>(prop) :> _ AutoSchemaField
        } |> Seq.map (fun f -> f.FieldName, f) |> dictionary
    let schemaFields =
        seq {
            for KeyValue(name, field) in fields ->
                name, field :> ISchemaField
        } |> dictionary
    override this.Select(instance : obj, context, fieldName, args, directives) =
        fields.[fieldName].Selector(instance, context, args, directives)
    override this.TypeName = name
    override this.Description = descr
    override this.Fields = upcast schemaFields

type GraphQLEntity<'cxt>() as this =
    let autoQueryType = TypeMapping<'cxt>.GetQueryableMapping(this.GetType())
    [<GraphQLIgnore>]
    member this.QueryType = autoQueryType :> ISchemaQueryType
    interface ISelectable<'cxt> with
        member this.Select
            ( context
            , fieldNAme
            , args
            , directives
            ) =
            autoQueryType.Select(this, context, fieldNAme, args, directives)
