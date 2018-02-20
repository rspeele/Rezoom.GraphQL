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
open System.Runtime.CompilerServices

// This modules defines types that represent a selection in a way easier for code executing a query to deal with.

type ExecArgument =
    {   Argument : ISchemaArgument
        Value : ConcreteValue
    }

type ExecDirective =
    {   SchemaDirective : ISchemaDirective
        Arguments : ExecArgument ListWithSource
    }

type ExecSelection =
    {   SchemaField : ISchemaField
        Alias : string option
        TypeCondition : ISchemaQueryType option
        Arguments : ExecArgument ListWithSource
        Directives : ExecDirective ListWithSource
        Selections : ExecSelection ListWithSource
    }
    member this.Name = defaultArg this.Alias this.SchemaField.FieldName

type IVariableSet =
    abstract member GetVariableValue : string -> ConcreteValue option

type DefaultExecContext() =
    interface IVariableSet with
        member this.GetVariableValue(_) = None
    static member Instance = new DefaultExecContext() :> IVariableSet
    

module private ToExecutionForm =
    let execArgument (context : IVariableSet) ({ Source = pos; Value = { Argument = argument; Expression = expr } }) =
        let argValue = expr.ToValue(context.GetVariableValue)
        if not <| argument.ArgumentType.AcceptsValue(argValue) then
            failAt pos (sprintf "unacceptable value for argument ``%s''" argument.ArgumentName)
        {   Source = pos
            Value =
            {   Argument = argument
                Value = argValue
            }
        }
    let execDirective (context : IVariableSet) (directive : Directive) =
        {   SchemaDirective = directive.SchemaDirective
            Arguments = directive.Arguments |> Seq.map (execArgument context) |> toReadOnlyList
        }
    let rec execSelections (context : IVariableSet) (selection : Selection WithSource) =
        match selection.Value with
        | FieldSelection field ->
            {   SchemaField = field.SchemaField
                Alias = field.Alias
                TypeCondition = None
                Arguments =
                    field.Arguments |> Seq.map (execArgument context) |> toReadOnlyList
                Directives =
                    field.Directives |> mapWithSource (execDirective context) |> toReadOnlyList
                Selections =
                    field.Selections
                    |> Seq.collect (execSelections context)
                    |> toReadOnlyList
            } |> (fun s -> { Source = selection.Source; Value = s }) |> Seq.singleton
        | FragmentSpreadSelection spread ->
            let spreadDirs = spread.Directives |> mapWithSource (execDirective context) |> toReadOnlyList
            let subMap (sel : ExecSelection) = 
                { sel with 
                    Directives = appendReadOnlyList sel.Directives spreadDirs 
                    TypeCondition = match sel.TypeCondition with None -> spread.Fragment.TypeCondition |> Some | Some _ -> sel.TypeCondition
                }
            spread.Fragment.Selections
            |> Seq.collect (execSelections context >> mapWithSource subMap)
        | InlineFragmentSelection frag ->
            let fragDirs = frag.Directives |> mapWithSource (execDirective context) |> toReadOnlyList
            let subMap (sel : ExecSelection) =
                { sel with
                    Directives = appendReadOnlyList sel.Directives fragDirs
                    TypeCondition = match sel.TypeCondition with None -> frag.TypeCondition | Some _ -> sel.TypeCondition
                }
            frag.Selections
            |> Seq.collect (execSelections context >> mapWithSource subMap)

[<Extension>]
type ExecContextExtensions =
    [<Extension>]
    static member ToExecSelections(context : IVariableSet, selection : Selection WithSource) =
        ToExecutionForm.execSelections context selection
    [<Extension>]
    static member ToExecSelections(context : IVariableSet, selections : Selection ListWithSource) =
        selections |> Seq.collect (ToExecutionForm.execSelections context)
    [<Extension>]
    static member ToExecSelections(context : IVariableSet, operation : Operation) =
        match operation with
        | ShorthandOperation sels -> ExecContextExtensions.ToExecSelections(context, sels)
        // TODO propagate directives from top level
        | LonghandOperation op -> ExecContextExtensions.ToExecSelections(context, op.Selections)