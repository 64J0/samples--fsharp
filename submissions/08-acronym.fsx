module Acronym

open System

let abbreviate (phrase: string) : string =
    phrase.Split [| ' '; '-'; '_' |]
    |> Array.filter (String.IsNullOrWhiteSpace >> not)
    |> Array.map (Seq.head >> Char.ToUpper >> Char.ToString)
    |> String.concat ""

abbreviate "Something - I made up from thin air"
