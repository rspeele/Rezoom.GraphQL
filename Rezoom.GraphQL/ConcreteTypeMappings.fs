namespace Rezoom.GraphQL
open System
open System.Collections
open System.Collections.Generic
open FSharp.Reflection

type IConcreteTypeMapping =
    abstract CLRType : Type
    abstract ConcreteType : ConcreteType
    abstract ConcreteToObj : ConcreteValue -> obj
    abstract ObjToConcrete : obj -> ConcreteValue

[<AbstractClass>]
type ConcreteTypeMapping<'a>() =
    abstract ConcreteType : ConcreteType
    abstract FromValue : ConcreteValue -> 'a
    abstract ToValue : 'a -> ConcreteValue
    interface IConcreteTypeMapping with
        member this.CLRType = typeof<'a>
        member this.ConcreteType = this.ConcreteType
        member this.ConcreteToObj(value) = this.FromValue(value) |> box
        member this.ObjToConcrete(obj) = this.ToValue(Unchecked.unbox obj)

type IConcreteTypeMapper =
    abstract MapType : allTypes : IConcreteTypeMapper * ty : Type -> IConcreteTypeMapping option

module StandardTypeMappings =
    type Utils =
        static member ArrayToList<'a>(arr) = List.ofArray arr
        static member ObjToSome<'a>(it : 'a) = Some it
        static member ObjToSomeNullable(it : 'a) = Nullable<'a>(it)
    let badValue() = failwith "Bad value for primitive type"
    let mapTypeMethod = typeof<IConcreteTypeMapper>.GetMethod("MapType")
    let private intMapping =
        let concreteType =
            { new ISchemaConcreteType with
                member this.TypeName = "Int"
                member this.Description =
                    Some "The Int scalar type represents a signed 32‐bit numeric non‐fractional value."
                member this.CoreType = ScalarType IntType
                member this.ValidateValue(v) =
                    match v with
                    | ScalarValue (IntScalar i) -> i > int64 Int32.MinValue && i < int64 Int32.MaxValue
                    | _ -> false
            } |> NamedType |> ConcreteType
        { new ConcreteTypeMapping<int>() with
            override this.ConcreteType = concreteType
            override this.FromValue(v) =
                match v with
                | ScalarValue (IntScalar i) -> Checked.int i
                | _ -> badValue()
            override this.ToValue(i) =
                ScalarValue (IntScalar (int64 i))
        }
    let private stringMapping =
        let concreteType =
            { new ISchemaConcreteType with
                member this.TypeName = "String"
                member this.Description =
                    Some (
                        "The String scalar type represents textual data, represented as UTF‐8 character sequences."
                        + " The String type is most often used by GraphQL to represent free‐form human‐readable text."
                    )
                member this.CoreType = ScalarType StringType
                member this.ValidateValue(_) = true
            } |> NamedType |> ConcreteType
        { new ConcreteTypeMapping<string>() with
            override this.ConcreteType = concreteType
            override this.FromValue(v) =
                match v with
                | ScalarValue (StringScalar s) -> s
                | _ -> badValue()
            override this.ToValue(s) =
                ScalarValue (StringScalar s)
        }
    let private boolMapping =
        let concreteType =
            { new ISchemaConcreteType with
                member this.TypeName = "Boolean"
                member this.Description = Some "The Boolean scalar type represents true or false."
                member this.CoreType = ScalarType BooleanType
                member this.ValidateValue(_) = true
            } |> NamedType |> ConcreteType
        { new ConcreteTypeMapping<bool>() with
            override this.ConcreteType = concreteType
            override this.FromValue(v) =
                match v with
                | ScalarValue (BooleanScalar b) -> b
                | _ -> badValue()
            override this.ToValue(b) =
                ScalarValue (BooleanScalar b)
        }
    let private unitMapping =
        let unitType = ConcreteType(ListType intMapping.ConcreteType)
        { new IConcreteTypeMapping with
            member this.CLRType = typeof<unit>
            member this.ConcreteType = unitType
            member this.ConcreteToObj(c) =
                match c with
                | ListValue xs when xs.Count = 0 -> box ()
                | _ -> badValue()
            member this.ObjToConcrete(o) =
                ListValue ([||] :> _ IReadOnlyList)
        }
    let standardMappingProvider =
        let them =
            [|  typeof<int>, intMapping :> IConcreteTypeMapping
                typeof<string>, stringMapping :> _
                typeof<bool>, boolMapping :> _
                typeof<unit>, unitMapping
                // TODO standard "ID" mapping for guids + extra GraphQLId class
            |] |> dictionary
        { new IConcreteTypeMapper with
            member this.MapType(_, ty) =
                let succ, found = them.TryGetValue(ty)
                if succ then
                    Some found
                else
                    None
        }
    let enumMappingProvider =
        { new IConcreteTypeMapper with
            member this.MapType(_, t) =
                if not t.IsEnum then None else
                let names = t.GetEnumNames()
                let enumType =
                    {   EnumName = t.Name
                        Description = None // TODO get description from attribute?
                        Values =
                            seq {
                                for name in names ->
                                    name,
                                        {   ValueName = name
                                            Description = None // TODO get description from attribute?
                                        }
                            } |> dictionary
                    }
                let concreteType =
                    let typeName = GraphQLNameAttribute.GetName(t)
                    let descr = GraphQLDescriptionAttribute.GetDescription(t)
                    { new ISchemaConcreteType with
                        member this.TypeName = typeName
                        member this.Description = descr
                        member this.CoreType = EnumType enumType
                        member this.ValidateValue(_) = true
                    } |> NamedType |> ConcreteType
                { new IConcreteTypeMapping with
                    override this.CLRType = t
                    override this.ConcreteType = concreteType
                    override this.ConcreteToObj(v) =
                        match v with
                        | EnumValue e -> Enum.Parse(t, e, ignoreCase = false) |> unbox
                        | _ -> badValue()
                    override this.ObjToConcrete(e) =
                        EnumValue (e.ToString())
                } |> Some
        }
    let enumerableMappingProvider =
        { new IConcreteTypeMapper with
            member this.MapType(mapper, ty) =
                match sequenceElementType ty with
                | Some (ty, elementType) ->
                    match mapper.MapType(mapper, elementType) with
                    | Some elementMapping ->
                        let seqType = typedefof<_ seq>.MakeGenericType(elementType)
                        let ctor = ty.GetConstructor([| seqType |])
                        if isNull ctor then None else
                        // claim it's non-nullable... we can handle nulls for output though
                        let concreteType = ConcreteType(ListType elementMapping.ConcreteType, false)
                        let seqCast = typeof<System.Linq.Enumerable>.GetMethod("Cast").MakeGenericMethod(elementType)
                        { new IConcreteTypeMapping with
                            override this.CLRType = ty
                            override this.ConcreteType = concreteType  
                            override this.ObjToConcrete(o) =
                                match o with
                                | :? IEnumerable as e ->
                                    seq {
                                        for (x : obj) in e ->
                                            { Source = SourceInfo.Artificial; Value = elementMapping.ObjToConcrete(x) }
                                    } |> ResizeArray :> _ IReadOnlyList |> ListValue
                                | null -> NullValue
                                | _ -> badValue()
                            override this.ConcreteToObj(c) =
                                match c with
                                | ListValue xs ->
                                    let seqOfObjs =
                                        seq {
                                            for { Value = v } in xs ->
                                                elementMapping.ConcreteToObj(v)
                                        }
                                    let seqOfElements = seqCast.Invoke(null, [| seqOfObjs |])
                                    ctor.Invoke([| seqOfElements |])
                                | _ -> badValue()
                        } |> Some
                    | None -> None
                | None -> None
        }
    let arrayMappingProvider =
        { new IConcreteTypeMapper with
            member this.MapType(mapper, ty) =
                if not ty.IsArray then None else
                let elementType = ty.GetElementType()
                match mapper.MapType(mapper, elementType) with
                | Some elementMapping ->
                    let concreteType = ConcreteType(ListType elementMapping.ConcreteType, false)
                    { new IConcreteTypeMapping with
                        override this.CLRType = ty
                        override this.ConcreteType = concreteType
                        override this.ObjToConcrete(o) =
                            match o with
                            | :? Array as arr ->
                                seq {
                                    for (x : obj) in arr ->
                                        { Source = SourceInfo.Artificial; Value = elementMapping.ObjToConcrete(x) }
                                } |> ResizeArray :> _ IReadOnlyList |> ListValue
                            | _ -> badValue()
                        override this.ConcreteToObj(c) =
                            match c with
                            | ListValue xs ->
                                let arr = Array.CreateInstance(elementType, xs.Count)
                                let mutable i = 0
                                for { Value = x } in xs do
                                    let elem = elementMapping.ObjToConcrete(x)
                                    arr.SetValue(elem, i)
                                    i <- i + 1
                                box arr
                            | _ -> badValue()
                    } |> Some
                | None -> None
        }
    let listMappingProvider =
        let listTypeDef = typedefof<_ list>
        let isList (ty : Type) =
            ty.IsGenericType && obj.ReferenceEquals(listTypeDef, ty.GetGenericTypeDefinition())
        { new IConcreteTypeMapper with
            member this.MapType(mapper, ty) =
                if not (isList ty) then None else
                let elementType = ty.GetGenericArguments().[0]
                match mapper.MapType(mapper, elementType) with
                | Some elementMapping ->
                    let concreteType = ConcreteType(ListType elementMapping.ConcreteType, false)
                    let arrayToList = typeof<Utils>.GetMethod("ArrayToList").MakeGenericMethod(elementType)
                    { new IConcreteTypeMapping with
                        override this.CLRType = ty
                        override this.ConcreteType = concreteType
                        override this.ObjToConcrete(o) =
                            match o with
                            | :? IEnumerable as e ->
                                seq {
                                    for (x : obj) in e ->
                                        { Source = SourceInfo.Artificial; Value = elementMapping.ObjToConcrete(x) }
                                } |> ResizeArray :> _ IReadOnlyList |> ListValue
                            | null -> NullValue
                            | _ -> badValue()
                        override this.ConcreteToObj(c) =
                            match c with
                            | ListValue xs ->
                                let arr = Array.CreateInstance(elementType, xs.Count)
                                let mutable i = 0
                                for { Value = x } in xs do
                                    let elem = elementMapping.ObjToConcrete(x)
                                    arr.SetValue(elem, i)
                                    i <- i + 1
                                arrayToList.Invoke(null, [| arr |])
                            | _ -> badValue()
                    } |> Some
                | None -> None
        }
    let optionMappingProvider =
        let optionTypeDef = typedefof<_ option>
        let isOption (ty : Type) =
            ty.IsGenericType && obj.ReferenceEquals(optionTypeDef, ty.GetGenericTypeDefinition())
        { new IConcreteTypeMapper with
            member this.MapType(mapper, ty) =
                if not (isOption ty) then None else
                let elementType = ty.GetGenericArguments().[0]
                match mapper.MapType(mapper, elementType) with
                | Some elementMapping ->
                    let some = typeof<Utils>.GetMethod("ObjToSome").MakeGenericMethod(elementType)
                    let getValue = ty.GetProperty("Value")
                    if elementMapping.ConcreteType.Nullable then
                        // we have to double-bag if our element could also be null, so we can distinguish between
                        // null on the inside and null on the outside... so we wrap in a list which could itself be null
                        let concreteType = ConcreteType(ListType elementMapping.ConcreteType, true)
                        { new IConcreteTypeMapping with
                            override this.CLRType = ty
                            override this.ConcreteType = concreteType
                            override this.ObjToConcrete(o) =
                                if isNull o then
                                    NullValue
                                else
                                    let inner = elementMapping.ObjToConcrete(o)
                                    [| { Source = SourceInfo.Artificial; Value = inner } |]
                                    :> _ IReadOnlyList
                                    |> ListValue
                            override this.ConcreteToObj(c) =
                                match c with
                                | NullValue -> null
                                | ListValue xs ->
                                    if xs.Count <> 1 then badValue() else
                                    let it = elementMapping.ConcreteToObj(xs.[0].Value)
                                    some.Invoke(null, [| it |])
                                | _ -> badValue()
                        } |> Some
                    else
                        // if the element type is not considered nullable we don't have to wrap it, we
                        // just have to handle outputting/inputting the null case
                        let concreteType = ConcreteType(elementMapping.ConcreteType.Type, true)
                        { new IConcreteTypeMapping with
                            override this.CLRType = ty
                            override this.ConcreteType = concreteType
                            override this.ObjToConcrete(o) =
                                if isNull o then NullValue else
                                let unwrapped = getValue.GetValue(o)
                                elementMapping.ObjToConcrete(unwrapped)
                            override this.ConcreteToObj(c) =
                                match c with
                                | NullValue -> null // thankfully None options are just represented by actual nulls
                                | notNull ->
                                    let it = elementMapping.ConcreteToObj(notNull)
                                    some.Invoke(null, [| it |])
                        } |> Some
                | None -> None
        }
    let nullableMappingProvider =
        let nullableTypeDef = typedefof<_ Nullable>
        let isNullable (ty : Type) =
            ty.IsValueType && ty.IsGenericType && obj.ReferenceEquals(nullableTypeDef, ty.GetGenericTypeDefinition())
        { new IConcreteTypeMapper with
            member this.MapType(mapper, ty) =
                if not (isNullable ty) then None else
                let elementType = ty.GetGenericArguments().[0]
                match mapper.MapType(mapper, elementType) with
                | Some elementMapping ->
                    let some = typeof<Utils>.GetMethod("ObjToSomeNullable").MakeGenericMethod(elementType)
                    if elementMapping.ConcreteType.Nullable then
                        // we have to double-bag if our element could also be null, so we can distinguish between
                        // null on the inside and null on the outside... so we wrap in a list which could itself be null
                        let concreteType = ConcreteType(ListType elementMapping.ConcreteType, true)
                        { new IConcreteTypeMapping with
                            override this.CLRType = ty
                            override this.ConcreteType = concreteType
                            override this.ObjToConcrete(o) =
                                if isNull o then
                                    NullValue
                                else
                                    let inner = elementMapping.ObjToConcrete(o)
                                    [| { Source = SourceInfo.Artificial; Value = inner } |]
                                    :> _ IReadOnlyList
                                    |> ListValue
                            override this.ConcreteToObj(c) =
                                match c with
                                | NullValue -> null
                                | ListValue xs ->
                                    if xs.Count <> 1 then badValue() else
                                    let it = elementMapping.ConcreteToObj(xs.[0].Value)
                                    some.Invoke(null, [| it |])
                                | _ -> badValue()
                        } |> Some
                    else
                        // if the element type is not considered nullable we don't have to wrap it, we
                        // just have to handle outputting/inputting the null case
                        let concreteType = ConcreteType(elementMapping.ConcreteType.Type, true)
                        { new IConcreteTypeMapping with
                            override this.CLRType = ty
                            override this.ConcreteType = concreteType
                            override this.ObjToConcrete(o) =
                                if isNull o then NullValue else
                                elementMapping.ObjToConcrete(o)
                            override this.ConcreteToObj(c) =
                                match c with
                                | NullValue -> null // thankfully null nullables are just represented by actual nulls
                                | notNull ->
                                    let it = elementMapping.ConcreteToObj(notNull)
                                    some.Invoke(null, [| it |])
                        } |> Some
                | None -> None
        }
    let discriminatedUnionProvider =
        { new IConcreteTypeMapper with
            member this.MapType(_, ty) =
                if not <| FSharpType.IsUnion(ty) then None else
                let cases = FSharpType.GetUnionCases(ty)
                // for now don't bother supporting union cases with fields, though that is definitely needed
                // in the future!
                if cases |> Array.exists(fun c -> c.GetFields().Length > 0) then None else
                let enumType =
                    {   EnumName = ty.Name
                        Description = None // TODO get description from attribute?
                        Values =
                            seq {
                                for case in cases ->
                                    case.Name,
                                        {   ValueName = case.Name
                                            Description = None // TODO get description from attribute?
                                        }
                            } |> dictionary
                    }
                let concreteType =
                    { new ISchemaConcreteType with
                        member this.TypeName = ty.Name
                        member this.Description = None
                        member this.CoreType = EnumType enumType
                        member this.ValidateValue(_) = true
                    } |> NamedType |> ConcreteType
                let tagProp = ty.GetProperty("Tag")
                let lookupByTag =
                    seq {
                        for case in cases ->
                            case.Tag, EnumValue case.Name
                    } |> dictionary
                let lookupByName =
                    seq {
                        for case in cases ->
                            case.Name, FSharpValue.MakeUnion(case, [||])
                    } |> dictionary
                { new IConcreteTypeMapping with
                    override this.CLRType = ty
                    override this.ConcreteType = concreteType
                    override this.ObjToConcrete(o) =
                        let tag = match tagProp.GetValue(o) with | :? int as i -> i | _ -> badValue()
                        lookupByTag.[tag]
                    override this.ConcreteToObj(c) =
                        match c with
                        | EnumValue name ->
                            lookupByName.[name]
                        | _ -> badValue()
                } |> Some
        }
    let singleCaseWrapperProvider =
        { new IConcreteTypeMapper with
            member this.MapType(mapper, ty) =
                if not <| FSharpType.IsUnion(ty) then None else
                let cases = FSharpType.GetUnionCases(ty)
                match cases with
                | [| singleCase |] ->
                    match singleCase.GetFields() with
                    | [| singleField |] ->
                        match mapper.MapType(mapper, singleField.PropertyType) with
                        | Some elementMapping ->
                            let concreteType =
                                { new ISchemaConcreteType with
                                    member this.TypeName = ty.Name
                                    member this.Description = None
                                    member this.CoreType = elementMapping.ConcreteType.Type
                                    member this.ValidateValue(_) = true
                                } |> NamedType |> fun n -> ConcreteType(n, elementMapping.ConcreteType.Nullable)
                            { new IConcreteTypeMapping with
                                override this.CLRType = ty
                                override this.ConcreteType = concreteType
                                override this.ObjToConcrete(o) =
                                    elementMapping.ObjToConcrete(singleField.GetValue(o))
                                override this.ConcreteToObj(c) =
                                    FSharpValue.MakeUnion(singleCase, [| elementMapping.ConcreteToObj(c) |])
                            } |> Some
                        | None -> None
                    | _ -> None
                | _ -> None
        }