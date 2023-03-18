module Anagram

let sortString (input: string) =
    input.ToLower()
    |> Seq.sort
    |> Seq.toList

let findAnagrams (candidates: string list) (target: string) =
    candidates
    |> List.filter (fun candidate -> candidate.Length = target.Length)
    |> List.filter (fun candidate -> candidate.ToUpper() <> target.ToUpper())
    |> List.filter (fun candidate -> (sortString candidate) = (sortString target))

findAnagrams ["stone"; "tones"; "banana"; "tons"; "notes"; "Seton"] "stone"

