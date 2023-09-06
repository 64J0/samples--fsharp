module Seq

let initialState: list<'T> = List.empty

let keep (pred: 'T -> bool) (xs: seq<'T>) =
    xs
    |> Seq.toList
    |> List.fold
        (fun (state: list<'T>) (x: 'T) -> if (pred x) then ([ x ] |> List.append state) else state)
        initialState

let discard (pred: 'T -> bool) (xs: seq<'T>) =
    xs
    |> Seq.toList
    |> List.fold
        (fun (state: list<'T>) (x: 'T) -> if not (pred x) then ([ x ] |> List.append state) else state)
        initialState

keep (fun x -> x < 10) []

set [ 1; 2; 3 ] |> keep (fun x -> x < 10) |> Seq.toList

[| 1; 2; 3 |] |> keep (fun x -> x % 2 <> 0) |> Seq.toList
