namespace Rezoom.GraphQL
open Rezoom.GraphQL.Execution

type ValidatedDocument(schema : ISchema, query : string) =
    let document = Parser.parseDocument query
    let documentContext = SchemaResolver.DocumentContext(schema, document)
    let operations = documentContext.ResolveOperations()
    member this.Execute(variables : IVariableSet, context : 'cxt, schema : ISchemaSelectable<'cxt>, operationName : string option) =
        let operation =
            match operationName with
            | None -> operations.[0].Value
            | Some name ->
                operations
                |> Seq.find (fun o -> match o.Value.Name with | Some n -> n = name | None -> false)
                |> fun o -> o.Value
        let selections = variables.ToExecSelections(operation) |> ResizeArray
        let selectable =
            match operation.OperationType with
            | Query -> schema.QuerySelectable
            | Mutation -> schema.MutationSelectable
        executeObject context selectable selections
