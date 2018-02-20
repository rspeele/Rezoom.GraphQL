﻿//MIT License
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

[<AutoOpen>]
module private Rezoom.GraphQL.Utilities
open Rezoom.GraphQL
open System
open System.Reflection
open System.Collections
open System.Collections.Generic

/// Make a dictionary from a list of key/value pair tuples.
/// This is like the built-in F# function dict, except it returns a System.Collections.Dictionary
/// instead of an IDictionary, and it does *not* allow duplicate keys to implicitly overwrite each
/// other.
let dictionary (pairs : ('k * 'v) seq) =
    let d = new Dictionary<_, _>()
    for key, value in pairs do
        d.Add(key, value)
    d

type IReadOnlyDictionary<'k, 'v> with
    /// Return `Some value` if the key is present in the dictionary, otherwise `None`.
    member this.TryFind(key : 'k) =
        let mutable output = Unchecked.defaultof<'v>
        if this.TryGetValue(key, &output) then Some output
        else None

let invalid msg =
    raise (new ValidationException(msg))

let failAt pos msg =
    raise (new SourceException(msg, pos))

let chooseWithSource transform inputs =
    seq {
        for { Source = pos; Value = v } in inputs do
            match transform v with
            | None -> ()
            | Some tr ->
                yield { Source = pos; Value = tr }
    }

let mapWithSource transform inputs =
    seq {
        for { Source = pos; Value = v } in inputs do
            yield { Source = pos; Value = transform v }
    }

let collectWithSource transform inputs =
    inputs
    |> mapWithSource transform
    |> Seq.collect
        (function { Source = pos; Value = vs } -> vs |> Seq.map (fun v -> { Source = pos; Value = v }))

let mapDictionaryWithSource transform inputs =
    seq {
        for KeyValue(name, { Source = pos; Value = v }) in inputs do
            yield name, { Source = pos; Value = transform v }
    } |> dictionary :> IReadOnlyDictionary<_, _>

let toReadOnlyList xs = xs |> Seq.toArray :> IReadOnlyList<_>

let appendReadOnlyList xs ys = Seq.append xs ys |> toReadOnlyList

let obj2option x =
    if obj.ReferenceEquals(x, null) then None
    else Some x

[<GeneralizableValue>]
let emptyDictionary<'k, 'v when 'k : equality> : IReadOnlyDictionary<'k, 'v> =
    [||] |> dictionary :> IReadOnlyDictionary<'k, 'v>

let sequenceElementType (ty : Type) =
    if not <| typeof<IEnumerable>.IsAssignableFrom(ty) then None
    else
        let seqTypeDef = typedefof<_ seq>
        if ty.IsGenericType && ty.IsInterface && ty.GetGenericTypeDefinition() = typedefof<_ seq> then
            Some (ty, ty.GetGenericArguments().[0])
        else
        let seqInterface =
            ty.GetInterfaces()
            |> Array.tryFind (fun i -> i.IsGenericType && obj.ReferenceEquals(seqTypeDef, i.GetGenericTypeDefinition()))
        match seqInterface with
        | Some s ->
            Some (ty, s.GetGenericArguments().[0])
        | None ->
            None
