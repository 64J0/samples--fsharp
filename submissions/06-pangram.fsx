module Pangram

open System

let isPangram (phrase: string): bool =
    let expected =
        ['a' .. 'z']
        |> Set.ofList

    let transformedPhraseConstituents =
        phrase
        |> String.filter Char.IsLetter
        |> Seq.distinct
        |> Seq.map Char.ToLower
        |> Seq.sort
        |> Set.ofSeq

    Set.isEmpty (expected - transformedPhraseConstituents)

isPangram "The quick brown fox jumps over the lazy dog."
