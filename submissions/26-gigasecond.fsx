module Gigasecond

open System

[<Literal>]
let GIGA_SECOND = 1_000_000_000

let add (beginDate: DateTime) : DateTime =
    GIGA_SECOND |> beginDate.AddSeconds

let result = add (DateTime(2011, 4, 25))
printfn "Result: %A" result
