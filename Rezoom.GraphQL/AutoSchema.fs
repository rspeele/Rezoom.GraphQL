namespace Rezoom.GraphQL
open System
open System.Collections.Generic
open Rezoom.GraphQL.AutoSchema
open Rezoom.GraphQL.Execution

type GraphQLRootQuery<'cxt>() as this =
    inherit GraphQLEntity<'cxt>()
    let introSchema = lazy Introspection.IntroSchema<'cxt>(this.SchemaType)
    member val internal SchemaType = Unchecked.defaultof<ISchema> with get, set
    [<GraphQLName("__schema")>]
    member this.Schema = introSchema.Value
    [<GraphQLName("__type")>]
    member this.Type(name : string) =
        introSchema.Value.Types
        |> Seq.tryFind (fun t ->
            match t.Name with
            | Some typeName when typeName = name -> true
            | _ -> false)

type AutoSchema<'cxt>(queryType : GraphQLRootQuery<'cxt>, mutationType : GraphQLEntity<'cxt>) as this =
    let seenTypeNames = HashSet<string>()
    let rec walk (queryType : ISchemaQueryType) =
        if not (seenTypeNames.Add(queryType.TypeName)) then () else
        for KeyValue(_, field) in queryType.Fields do
            match field.FieldType with
            | ConcreteField _ -> ()
            | QueryField q -> walk q.QueryType
    let rootQueryType = queryType.QueryType
    let rootMutationType = mutationType.QueryType
    do
        queryType.SchemaType <- this
        walk rootQueryType
        walk rootMutationType
    let queryTypes =
        let dict = Dictionary()
        for KeyValue(_, queryType) in TypeMapping<'cxt>.QueryTypes do
            let queryType = queryType :> ISchemaQueryType
            dict.Add(queryType.TypeName, queryType)
        dict
    let concreteTypes =
        let dict = Dictionary<string, ConcreteTypeCase>()
        for concreteType in TypeMapping<'cxt>.ConcreteTypes.Values do
            match concreteType with
            | None -> ()
            | Some concrete ->
                match concrete.ConcreteType.Type with
                | NamedType n as namedType ->
                    let exists, existing = dict.TryGetValue(n.TypeName)
                    if exists then
                        if n.CoreType <> existing.BottomType then
                            failwith "Same name used for two types"
                    else
                        dict.Add(n.TypeName, namedType)
                | _ -> ()
        dict
    interface ISchema with
        member this.QueryTypes = upcast queryTypes
        member this.ConcreteTypes = upcast concreteTypes
        member this.Directives = upcast Dictionary()
        member this.RootQueryType = rootQueryType
        member this.RootMutationType = rootMutationType
    interface ISchemaSelectable<'cxt> with
        member this.QuerySelectable = upcast queryType
        member this.MutationSelectable = upcast mutationType

