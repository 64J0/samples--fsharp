module Isograms

open System

let isIsogram (word: string): bool =
    let lettersOfWord =
        word
        |> Seq.filter Char.IsLetter
        |> Seq.map Char.ToLower
        |> Seq.toList

    lettersOfWord = (lettersOfWord |> List.distinct)

isIsogram "isogram"
