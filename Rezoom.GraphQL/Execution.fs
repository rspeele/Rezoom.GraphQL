module Rezoom.GraphQL.Execution
open Rezoom
open Rezoom.GraphQL
open System.Collections.Generic

type ISelectable<'cxt> =
    abstract Select
        : context : 'cxt
        * fieldName : string
        * args : ExecArgument ListWithSource
        * directives : ExecDirective ListWithSource
        -> FieldResolution<'cxt> Plan

and FieldResolution<'cxt> =
    | ResolvedConcrete of ConcreteValue
    | ResolvedSelectable of ISelectable<'cxt>
    | ResolvedSelectables of ISelectable<'cxt> seq

let rec execute context (selectable : ISelectable<_>) (execSelection : ExecSelection WithSource) : ConcreteValue Plan =
    plan {
        let selection = execSelection.Value
        let! resolved =
            selectable.Select
                (context, selection.SchemaField.FieldName, selection.Arguments, selection.Directives)
        match resolved with
        | ResolvedConcrete fieldValue ->
            return fieldValue
        | ResolvedSelectable it ->
            return! executeObject context it selection.Selections
        | ResolvedSelectables them ->
            let! children =
                [ for it in them ->
                    plan {
                        let! complex = executeObject context it selection.Selections
                        return { Source = execSelection.Source; Value = complex }
                    }
                ] |> Plan.concurrentList
            return ListValue (List.toArray children :> _ IReadOnlyList)
    }

and executeObject context selectable (selections : ExecSelection ListWithSource) =
    plan {
        let! children =
            [ for childSelection in selections ->
                plan {
                    let! child = execute context selectable childSelection
                    let fieldName =
                        defaultArg
                            childSelection.Value.Alias
                            childSelection.Value.SchemaField.FieldName
                    return fieldName, { Source = childSelection.Source; Value = child }
                }
            ] |> Plan.concurrentList
        return children |> dictionary :> IReadOnlyDictionary<_, _> |> ObjectValue
    }

type ISchemaSelectable<'cxt> =
    abstract QuerySelectable : ISelectable<'cxt>
    abstract MutationSelectable : ISelectable<'cxt>