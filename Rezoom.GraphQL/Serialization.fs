module Rezoom.GraphQL.Serialization
open System.Text

let private escapeStringContents (str : string) =
    let builder = StringBuilder()
    for c in str do
        match c with
        | '"' -> ignore <| builder.Append("\\\"")
        | '\\' -> ignore <| builder.Append("\\\\")
        | c when  c >= ' ' && c <= '~' -> ignore <| builder.Append(c)
        | '\b' -> ignore <| builder.Append("\\b")
        | '\f' -> ignore <| builder.Append("\\f")
        | '\n' -> ignore <| builder.Append("\\n")
        | '\r' -> ignore <| builder.Append("\\r")
        | '\t' -> ignore <| builder.Append("\\t")
        | other ->
            let codePoint = int other
            let hex = codePoint.ToString("X4")
            ignore <| builder.Append("\\u")
            ignore <| builder.Append(hex)
    builder.ToString()

let escapeString str = "\"" + escapeStringContents str + "\""

let rec json (value : ConcreteValue) =
    match value with
    | ScalarValue p ->
        match p with
        | IntScalar i -> string i
        | FloatScalar f ->
            if System.Double.IsNaN(f) || System.Double.IsInfinity(f) then
                failwithf "Cannot serialize a floating-point %O to legal JSON" f
            else
                string f
        | StringScalar s -> escapeString s
        | BooleanScalar b -> if b then "true" else "false"
    | NullValue -> "null"
    | EnumValue ev -> escapeString ev
    | ListValue vs ->
        let commaSep =
            seq {
                for { Value = v } in vs ->
                    json v
            } |> String.concat ", "
        "[" + commaSep + "]"
    | ObjectValue kvs ->
        let commaSep =
            seq {
                for KeyValue(key, { Value = v }) in kvs ->
                    escapeString key + ": " + json v
            } |> String.concat ", "
        "{" + commaSep + "}"