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

namespace Rezoom.GraphQL.Introspection
open Rezoom.GraphQL
open Rezoom.GraphQL.Parser
open System.Collections.Generic

// This module implements the introspection schema described in
// section 4.2 of the GraphQL spec.

[<GraphQLName("__TypeKind")>]
type TypeKind =
    | SCALAR = 1
    | OBJECT = 2
    | INTERFACE = 3
    | UNION = 4
    | ENUM = 5
    | INPUT_OBJECT = 6
    | LIST = 7
    | NON_NULL = 8

[<GraphQLName("__DirectiveLocation")>]
type DirectiveLocation =
    | QUERY = 1
    | MUTATION = 2
    | FIELD = 3
    | FRAGMENT_DEFINITION = 4
    | FRAGMENT_SPREAD = 5
    | INLINE_FRAGMENT = 6

[<GraphQLName("__Type")>]
type IntroType<'cxt>
    ( kind : TypeKind
    , name : string option
    , description : string option
    , fields : IntroField<'cxt> seq option
    , interfaces : IntroType<'cxt> seq option
    , possibleTypes : IntroType<'cxt> seq option
    , enumValues : IntroEnumValue<'cxt> seq option
    , inputFields : IntroInputValue<'cxt> seq option
    , ofType : IntroType<'cxt> option
    ) =
    inherit AutoSchema.GraphQLEntity<'cxt>()
    [<GraphQLName("kind")>]
    member this.Kind = kind
    [<GraphQLName("name")>]
    member this.Name = name
    [<GraphQLName("description")>]
    member this.Description = description
    [<GraphQLName("fields")>]
    member this.Fields(includeDeprecated : bool option) =
        match fields, includeDeprecated with
        | None, _ -> None
        | Some fields, Some true -> Some fields
        | Some fields, None
        | Some fields, Some false -> Some (fields |> Seq.filter (fun f -> not f.IsDeprecated))
    [<GraphQLName("interfaces")>]
    member this.Interfaces = interfaces
    [<GraphQLName("possibleTypes")>]
    member this.PossibleTypes = possibleTypes
    [<GraphQLName("enumValues")>]
    member this.EnumValues(includeDeprecated : bool option) = enumValues
    [<GraphQLName("inputFields")>]
    member this.InputFields = inputFields
    [<GraphQLName("ofType")>]
    member this.OfType = ofType
    static member Default =
        IntroType<'cxt>(TypeKind.SCALAR, None, None, None, None, None, None, None, None)
    static member ListOf(inner : IntroType<'cxt>) =
        IntroType<'cxt>
            ( kind = TypeKind.LIST
            , name = None
            , description = None
            , fields = None
            , interfaces = None
            , possibleTypes = None
            , enumValues = None
            , inputFields = None
            , ofType = Some inner
            )
    static member NonNullOf(inner : IntroType<'cxt>) =
        IntroType<'cxt>
            ( kind = TypeKind.NON_NULL
            , name = None
            , description = None
            , fields = None
            , interfaces = None
            , possibleTypes = None
            , enumValues = None
            , inputFields = None
            , ofType = Some inner
            )
    static member Of(coreType : ConcreteTypeCase) =
        match coreType with
        | ScalarType _ -> IntroType<'cxt>.Default
        | NamedType namedType ->
            let i = IntroType<'cxt>.Of(namedType.CoreType)
            IntroType<'cxt>
                ( kind = i.Kind
                , name = Some namedType.TypeName
                , description = namedType.Description
                , fields = i.Fields(includeDeprecated = None)
                , interfaces = i.Interfaces
                , possibleTypes = i.PossibleTypes
                , enumValues = i.EnumValues(includeDeprecated = None)
                , inputFields = i.InputFields
                , ofType = i.OfType
                )
        | EnumType enumType ->
            IntroType<'cxt>
                ( kind = TypeKind.ENUM
                , name = Some enumType.EnumName
                , description = enumType.Description
                , fields = None
                , interfaces = None
                , possibleTypes = None
                , enumValues = (enumType.Values.Values |> Seq.map IntroEnumValue.Of |> Some)
                , inputFields = None
                , ofType = None
                )
        | ListType elementType ->
            let i = IntroType<'cxt>.Of(elementType)
            IntroType<'cxt>.ListOf(i)
        | ObjectType fieldTypes ->
            IntroType<'cxt>
                ( kind = TypeKind.INPUT_OBJECT
                , name = None
                , description = None
                , fields = None
                , interfaces = None
                , possibleTypes = None
                , enumValues = None
                , inputFields = (fieldTypes |> Seq.map IntroInputValue<'cxt>.Of |> Some)
                , ofType = None
                )
    static member Of(varType : ConcreteType) : IntroType<'cxt> =
        if not varType.Nullable then
            IntroType<'cxt>.NonNullOf(IntroType<'cxt>.Of(varType.Type))
        else
            IntroType<'cxt>.Of(varType.Type)
    static member Of(queryType : ISchemaQueryType) =
        let fields = queryType.Fields.Values |> Seq.map IntroField<'cxt>
        IntroType<'cxt>
            ( kind = TypeKind.OBJECT
            , name = Some queryType.TypeName
            , description = queryType.Description
            , fields = Some fields
            , interfaces = Some Seq.empty
            , possibleTypes = None
            , enumValues = None
            , inputFields = None
            , ofType = None
            )
    static member Of(fieldType : SchemaFieldType) =
        match fieldType with
        | QueryField qty ->
            let queryType = IntroType<'cxt>.Of(qty.QueryType)
            let queryType =
                if qty.IsList then IntroType<'cxt>.ListOf(queryType) else queryType
            let queryType =
                if qty.IsOptional then queryType else IntroType<'cxt>.NonNullOf(queryType)
            queryType
        | ConcreteField vty -> IntroType<'cxt>.Of(vty)
   
and [<GraphQLName("__Field")>] IntroField<'cxt>(field : ISchemaField)=
    inherit AutoSchema.GraphQLEntity<'cxt>()
    let args = field.Arguments.Values |> Seq.map IntroInputValue<'cxt>.Of
    let ty = IntroType<'cxt>.Of(field.FieldType)
    [<GraphQLName("name")>]
    member this.Name = field.FieldName
    [<GraphQLName("description")>]
    member this.Description = field.Description
    [<GraphQLName("args")>]
    member this.Args = args
    [<GraphQLName("type")>]
    member this.Type = ty
    [<GraphQLName("isDeprecated")>]
    member this.IsDeprecated : bool = Option.isSome field.DeprecatedForReason
    [<GraphQLName("deprecationReason")>]
    member this.DeprecationReason = field.DeprecatedForReason

and [<GraphQLName("__InputValue")>] IntroInputValue<'cxt>
    ( name : string
    , description : string option
    , ty : IntroType<'cxt>
    , defaultValue : string option
    ) =
    inherit AutoSchema.GraphQLEntity<'cxt>()
    [<GraphQLName("name")>]
    member this.Name = name
    [<GraphQLName("description")>]
    member this.Description = description
    [<GraphQLName("type")>]
    member this.Type = ty
    [<GraphQLName("defaultValue")>]
    member this.DefaultValue = defaultValue
    static member Of(kv : KeyValuePair<string, ConcreteType>) : IntroInputValue<'cxt> =
        IntroInputValue<'cxt>(kv.Key, None, IntroType<'cxt>.Of(kv.Value), None)
    static member Of(arg : ISchemaArgument) : IntroInputValue<'cxt> =
        IntroInputValue<'cxt>(arg.ArgumentName, arg.Description, IntroType<'cxt>.Of(arg.ArgumentType), None)

and [<GraphQLName("__EnumValue")>] IntroEnumValue<'cxt>
    (name : string, description : string option, isDeprecated : bool, deprecationReason : string option) =
    inherit AutoSchema.GraphQLEntity<'cxt>()
    [<GraphQLName("name")>]
    member this.Name = name
    [<GraphQLName("description")>]
    member this.Description = description
    [<GraphQLName("isDeprecated")>]
    member this.IsDeprecated = isDeprecated
    [<GraphQLName("deprecationReason")>]
    member this.DeprecationReason = deprecationReason
    static member Of(enumValue : EnumTypeValue) : IntroEnumValue<'cxt> =
        IntroEnumValue<'cxt>
            ( name = enumValue.ValueName
            , description = None
            , isDeprecated = false
            , deprecationReason = None
            )

and [<GraphQLName("__Directive")>] IntroDirective<'cxt>(dir : ISchemaDirective) =
    inherit AutoSchema.GraphQLEntity<'cxt>()
    let name = dir.DirectiveName
    let description = dir.Description
    let locations = // TODO: let the schema directive say what locations it's valid in
            [   DirectiveLocation.QUERY
                DirectiveLocation.MUTATION
                DirectiveLocation.FIELD
                DirectiveLocation.FRAGMENT_DEFINITION
                DirectiveLocation.FRAGMENT_SPREAD
                DirectiveLocation.INLINE_FRAGMENT ]
    let args = dir.Arguments.Values |> Seq.map IntroInputValue<'cxt>.Of
    [<GraphQLName("name")>]
    member this.Name = name
    [<GraphQLName("description")>]
    member this.Description = description
    [<GraphQLName("locations")>]
    member this.Locations = locations
    [<GraphQLName("args")>]
    member this.Args = args

[<GraphQLName("__Schema")>]
type IntroSchema<'cxt>(schema : ISchema) =
    inherit AutoSchema.GraphQLEntity<'cxt>()
    let types =
        [   schema.ConcreteTypes.Values |> Seq.map IntroType<'cxt>.Of
            schema.QueryTypes.Values |> Seq.map IntroType<'cxt>.Of
        ] |> Seq.concat
    let queryType = IntroType<'cxt>.Of(schema.RootQueryType)
    let mutationType = Some <| IntroType<'cxt>.Of(schema.RootMutationType)
    let directives =
        schema.Directives.Values |> Seq.map IntroDirective<'cxt>
    [<GraphQLName("types")>]
    member this.Types = types
    [<GraphQLName("queryType")>]
    member this.QueryType = queryType
    [<GraphQLName("mutationType")>]
    member this.MutationType = mutationType
    [<GraphQLName("directives")>]
    member this.Directives = directives
