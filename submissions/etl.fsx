module Etl

let transform (scoresWithLetters: Map<int, char list>): Map<char, int> =
    scoresWithLetters
    |> Map.toList
    |> List.collect (fun (number, charList) -> charList |> List.map (fun c -> System.Char.ToLower c, number))
    |> Map.ofList
    
let lettersByScore = [(1, ['A'; 'E'; 'I'; 'O'; 'U'])] |> Map.ofList

printfn "%A" (transform lettersByScore)
