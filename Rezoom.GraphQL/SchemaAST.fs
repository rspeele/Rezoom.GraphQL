//MIT License
//
//Copyright (c) 2016 Robert Peele
//
//Permission is hereby granted, free of charge, to any person obtaining a copy
//of this software and associated documentation files (the "Software"), to deal
//in the Software without restriction, including without limitation the rights
//to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
//copies of the Software, and to permit persons to whom the Software is
//furnished to do so, subject to the following conditions:
//
//The above copyright notice and this permission notice shall be included in all
//copies or substantial portions of the Software.
//
//THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
//IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
//FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
//AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
//LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
//OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
//SOFTWARE.

namespace Rezoom.GraphQL
open Rezoom.GraphQL
open System.Collections.Generic

// This file implements an enhanced form of the AST with information added
// by validating it both for internal consistency and consistency with a schema.
// Generic types in this module with a `'s` generic parameter contain arbitrary information
// supplied by the schema implementation. For example, when the schema implementation is asked
// to resolve a type name, it can provide an `'s` with the resulting type information,
// which will be included in the AST for later use.

/// A scalar value, which needs no extra information.
type Scalar =
    | IntScalar of int64
    | FloatScalar of double
    | StringScalar of string
    | BooleanScalar of bool
    member this.Type =
        match this with
        | IntScalar _ -> IntType
        | FloatScalar _ -> FloatType
        | StringScalar _ -> StringType
        | BooleanScalar _ -> BooleanType
    member this.ToObject() =
        match this with
        | IntScalar i -> box i
        | FloatScalar f -> box f
        | StringScalar s -> box s
        | BooleanScalar b -> box b
and ScalarType =
    | IntType
    | FloatType
    | StringType
    | BooleanType
    member this.AcceptsScalar(input : ScalarType) =
        this = input
        || this = FloatType && input = IntType

type EnumTypeValue =
    {   ValueName : string
        Description : string option
    }
and EnumType =
    {   EnumName : string
        Description : string option
        Values : IReadOnlyDictionary<string, EnumTypeValue>
    }

type EnumValue = string

/// Represents an estimate of the complexity of a query operation, in
/// arbitrary units of work.
type Complexity = int64

type ListWithSource<'a> = IReadOnlyList<'a WithSource>

/// Represents type information provided by the schema implementation for validation
/// and for inclusion in the validated AST.
type ISchemaQueryType =
    abstract member TypeName : string
    abstract member Description : string option
    /// Get the fields of this type, keyed by name.
    /// May be empty, for example if the type is a primitive.
    abstract member Fields : IReadOnlyDictionary<string, ISchemaField>
/// Represents a named core type, e.g. a "Time" type represented by an ISO-formatted string.
/// The type may define validation rules that run on values after they have been checked to
/// match the given core type.
and ISchemaConcreteType =
    abstract member TypeName : string
    abstract member Description : string option
    abstract member CoreType : ConcreteTypeCase
    abstract member ValidateValue : ConcreteValue -> bool
and SchemaQueryFieldType =
    {   IsList : bool
        IsOptional : bool
        QueryType : ISchemaQueryType
    }
/// Represents the type of a field, which may be either another queryable type, or
/// a non-queryable value.
and SchemaFieldType =
    | QueryField of SchemaQueryFieldType
    | ConcreteField of ConcreteType
/// Represents field information provided by the schema implementation for validation
/// and for inclusion in the validated AST.
and ISchemaField =
    abstract member FieldType : SchemaFieldType
    abstract member FieldName : string
    abstract member Description : string option
    abstract member DeprecatedForReason : string option
    /// Get the possible arguments of this field, keyed by name.
    /// May be empty if the field accepts no arguments.
    abstract member Arguments : IReadOnlyDictionary<string, ISchemaArgument>
    /// Given the arguments (without values) supplied to this field, give a ballpark estimate of
    /// how much work it will take to select.
    abstract member EstimateComplexity : ISchemaArgument seq -> Complexity
/// Represents argument information provided by the schema implementation for validation
/// and for inclusion in the validated AST.
and ISchemaArgument =
    abstract member ArgumentName : string
    abstract member ArgumentType : ConcreteType
    abstract member Description : string option
and ISchemaDirective =
    abstract member DirectiveName : string
    abstract member Description : string option
    /// Get the possible arguments of this directive, keyed by name.
    /// May be empty if the directive accepts no arguments.
    abstract member Arguments : IReadOnlyDictionary<string, ISchemaArgument>
and ISchema =
    abstract member ConcreteTypes : IReadOnlyDictionary<string, ConcreteTypeCase>
    abstract member QueryTypes : IReadOnlyDictionary<string, ISchemaQueryType>
    abstract member Directives : IReadOnlyDictionary<string, ISchemaDirective>
    /// The top-level type that queries select from.
    /// Most likely this will correspond to your DB context type.
    abstract member RootQueryType : ISchemaQueryType
    abstract member RootMutationType : ISchemaQueryType
/// A value within the GraphQL document. This is fully resolved, not a variable reference.
and ConcreteValue =
    | ScalarValue of Scalar
    | NullValue
    | EnumValue of EnumValue
    | ListValue of ConcreteValue ListWithSource
    | ObjectValue of IReadOnlyDictionary<string, ConcreteValue WithSource>
    member this.GetString() =
        match this with
        | NullValue -> null
        | ScalarValue (StringScalar s) -> s
        | _ -> failwith "Value is not a string"
    member this.GetInteger() =
        match this with
        | ScalarValue (IntScalar i) -> i
        | _ -> failwith "Value is not an integer"
    member this.GetNumber() =
        match this with
        | ScalarValue (FloatScalar f) -> f
        | ScalarValue (IntScalar i) -> double i
        | _ -> failwith "Value is not a number"
    member this.GetBoolean() =
        match this with
        | ScalarValue (BooleanScalar b) -> b
        | _ -> failwith "Value is not a boolean"
    member this.ToExpression() =
        match this with
        | ScalarValue p -> ScalarExpression p
        | NullValue -> NullExpression
        | EnumValue e -> EnumExpression e
        | ListValue lst ->
            lst
            |> mapWithSource (fun v -> v.ToExpression())
            |> toReadOnlyList
            |> ListExpression
        | ObjectValue fields ->
            fields
            |> mapDictionaryWithSource (fun v -> v.ToExpression())
            |> ObjectExpression
/// A value expression within the GraphQL document.
/// This may contain references to variables, whose values are not yet
/// supplied.
and ValueExpression =
    | VariableExpression of VariableDefinition
    | ScalarExpression of Scalar
    | NullExpression
    | EnumExpression of EnumValue
    | ListExpression of ValueExpression ListWithSource
    | ObjectExpression of IReadOnlyDictionary<string, ValueExpression WithSource>
    member this.TryGetValue(resolveVariable) =
        match this with
        | VariableExpression vdef -> resolveVariable vdef.VariableName
        | ScalarExpression prim -> Some (ScalarValue prim)
        | NullExpression -> Some NullValue
        | EnumExpression en -> Some (EnumValue en)
        | ListExpression lst ->
            let mappedList =
                lst
                |> chooseWithSource (fun v -> v.TryGetValue(resolveVariable))
                |> toReadOnlyList
            if mappedList.Count < lst.Count
            then None
            else Some (ListValue mappedList)
        | ObjectExpression fields ->
            let mappedFields =
                seq {
                    for KeyValue(name, { Source = pos; Value = v }) in fields do
                        match v.TryGetValue(resolveVariable) with
                        | None -> ()
                        | Some res ->
                            yield name, { Source = pos; Value = res }
                } |> dictionary :> IReadOnlyDictionary<_, _>
            if mappedFields.Count < fields.Count
            then None
            else Some (ObjectValue mappedFields)
    member this.ToValue(resolveVariable) : ConcreteValue =
        match this with
        | VariableExpression vdef ->
            let attempt = resolveVariable vdef.VariableName
            match attempt with
            | Some v ->
                if vdef.VariableType.AcceptsValue(v) then v
                else failwith (sprintf "unacceptable value for variable ``%s''" vdef.VariableName)
            | None ->
                match vdef.DefaultValue with
                | None -> failwith (sprintf "no value provided for variable ``%s''" vdef.VariableName)
                | Some def -> def
        | ScalarExpression prim -> ScalarValue prim
        | NullExpression -> NullValue
        | EnumExpression en -> EnumValue en
        | ListExpression lst ->
            lst
            |> mapWithSource (fun v -> v.ToValue(resolveVariable))
            |> toReadOnlyList
            |> ListValue
        | ObjectExpression fields ->
            fields
            |> mapDictionaryWithSource (fun v -> v.ToValue(resolveVariable))
            |> ObjectValue 
/// Represents a non-nullable value type.
and ConcreteTypeCase =
    | ScalarType of ScalarType
    | EnumType of EnumType
    | ListType of ConcreteType
    /// Not possible to declare this type in a GraphQL document, but it exists nonetheless.
    | ObjectType of IReadOnlyDictionary<string, ConcreteType>
    | NamedType of ISchemaConcreteType
    member internal this.BottomType =
        match this with
        | NamedType n -> n.CoreType
        | _ -> this
    member this.Nullable(yes) = new ConcreteType(this, yes)
    member this.Nullable() = new ConcreteType(this, true)
    member this.NotNullable() = new ConcreteType(this, false)
    member this.TypeName =
        match this with
        | NamedType schemaVariableType -> Some schemaVariableType.TypeName
        | _ -> None
    member this.AcceptsVariableType(vtype : ConcreteTypeCase) =
        this = vtype ||
        match this, vtype with
        | ScalarType pTy, ScalarType inputTy -> pTy.AcceptsScalar(inputTy)
        | NamedType schemaType, vt ->
            schemaType.CoreType.AcceptsVariableType(vt)
        | ListType vt1, ListType vt2 ->
            (vt1.Type : ConcreteTypeCase).AcceptsVariableType(vt2)
        | ObjectType o1, ObjectType o2 ->
            seq {
                for KeyValue(name, vt1) in o1 do
                    yield
                        match o2.TryFind(name) with
                        | None -> false
                        | Some vt2 -> vt1.AcceptsVariableType(vt2)
            } |> Seq.forall id
        | _ -> false
    member this.AcceptsVariableType(vtype : ConcreteType) =
        this.AcceptsVariableType(vtype.Type)
    member this.AcceptsValue(value : ConcreteValue) =
        match this, value with
        | NamedType schemaType, value -> schemaType.CoreType.AcceptsValue(value) && schemaType.ValidateValue(value)
        | ScalarType pTy, ScalarValue pv -> pTy.AcceptsScalar(pv.Type)
        | EnumType eType, EnumValue eVal -> eType.Values.ContainsKey(eVal)
        | ListType lTy, ListValue vals -> vals |> Seq.forall (fun v -> lTy.AcceptsValue(v.Value))
        | ObjectType oTy, ObjectValue o ->
            seq {
                for KeyValue(name, ty) in oTy do
                    yield
                        match o.TryFind(name) with
                        | None -> false
                        | Some fv -> ty.AcceptsValue(fv.Value)
            } |> Seq.forall id
        | _ -> false
    member this.AcceptsValueExpression(vexpr : ValueExpression) =
        match vexpr.TryGetValue(fun _ -> None) with
        | Some value -> this.AcceptsValue(value)
        | None ->
        match this, vexpr with
        | NamedType schemaType, vexpr -> schemaType.CoreType.AcceptsValueExpression(vexpr)
        | ScalarType pTy, ScalarExpression pexpr -> pTy.AcceptsScalar(pexpr.Type)
        | EnumType eType, EnumExpression eVal -> eType.Values.ContainsKey(eVal)
        | ListType lTy, ListExpression vals -> vals |> Seq.forall (fun v -> lTy.AcceptsValueExpression(v.Value))
        | ObjectType oTy, ObjectExpression o ->
            seq {
                for KeyValue(name, ty) in oTy do
                    yield
                        match o.TryFind(name) with
                        | None -> false
                        | Some fv -> ty.AcceptsValueExpression(fv.Value)
            } |> Seq.forall id
        | _ -> false
and ConcreteType(coreType: ConcreteTypeCase, isNullable : bool) =
    new(coreType) = ConcreteType(coreType, false)
    member this.Type = coreType
    member this.Nullable = isNullable
    member this.AcceptsVariableType(vtype : ConcreteType) =
        this.Type.AcceptsVariableType(vtype)
    member this.AcceptsValueExpression(vexpr : ValueExpression) =
        match vexpr with
        | NullExpression -> this.Nullable
        | notNull -> this.Type.AcceptsValueExpression(notNull)
    member this.AcceptsValue(value : ConcreteValue) =
        match value with
        | NullValue -> this.Nullable
        | notNull -> this.Type.AcceptsValue(notNull)
and VariableDefinition =
    {   VariableName : string
        VariableType : ConcreteType
        DefaultValue : ConcreteValue option
    }

type ArgumentExpression =
    {   Argument : ISchemaArgument
        Expression : ValueExpression
    }

type Directive =
    {   SchemaDirective : ISchemaDirective
        Arguments : ArgumentExpression ListWithSource
    }

type Field =
    {   SchemaField : ISchemaField
        Alias : string option
        Arguments : ArgumentExpression ListWithSource
        Directives : Directive ListWithSource
        Selections : Selection ListWithSource
    }
and Selection =
    | FieldSelection of Field
    | FragmentSpreadSelection of FragmentSpread
    | InlineFragmentSelection of InlineFragment
and FragmentSpread =
    {   Fragment : Fragment
        Directives : Directive ListWithSource
    }
and Fragment =
    {   FragmentName : string
        TypeCondition : ISchemaQueryType
        Directives : Directive ListWithSource
        Selections : Selection ListWithSource
    }
and InlineFragment =
    {   TypeCondition : ISchemaQueryType option
        Directives : Directive ListWithSource
        Selections : Selection ListWithSource
    }

type OperationType =
    | Query
    | Mutation

type LonghandOperation =
    {   OperationType : OperationType
        OperationName : string option
        VariableDefinitions : VariableDefinition ListWithSource
        Directives : Directive ListWithSource
        Selections : Selection ListWithSource
    }

type Operation =
    | ShorthandOperation of Selection ListWithSource
    | LonghandOperation of LonghandOperation
    member this.OperationType =
        match this with
        | ShorthandOperation _ -> Query
        | LonghandOperation o -> o.OperationType
    static member private EstimateComplexity(selections : Selection ListWithSource) =
        selections
        |> Seq.map (fun s -> s.Value)
        |> Seq.sumBy Operation.EstimateComplexity 
    static member private EstimateComplexity(selection : Selection) =
        match selection with
        | FieldSelection field ->
            let fieldComplexity =
                field.SchemaField.EstimateComplexity(field.Arguments |> Seq.map (fun a -> a.Value.Argument))
            let subComplexity =
                Operation.EstimateComplexity(field.Selections)
            fieldComplexity + fieldComplexity * subComplexity
        | FragmentSpreadSelection spread -> Operation.EstimateComplexity(spread.Fragment.Selections)
        | InlineFragmentSelection frag -> Operation.EstimateComplexity(frag.Selections)
    member this.EstimateComplexity() =
        match this with
        | ShorthandOperation sels -> Operation.EstimateComplexity(sels)
        | LonghandOperation op -> Operation.EstimateComplexity(op.Selections)
    member this.Name =
        match this with
        | ShorthandOperation _ -> None
        | LonghandOperation op -> op.OperationName

// Note: we don't include fragment definitions in the schema-validated AST.
// This is because the Fragment<'s> type only makes sense where a fragment is
// used via the spread operator in an operation. It's impossible to resolve variable
// types against the schema at the point where a fragment is defined, because they could
// be different depending on where it's used.