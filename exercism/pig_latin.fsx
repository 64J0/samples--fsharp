open System
open System.Text.RegularExpressions

let private applyIfNotChanged (ruleFn: string -> string) (input: string, changed: bool) : string * bool =
    if changed then
        input, changed
    else
        let updatedInput = ruleFn input

        if updatedInput <> input then
            updatedInput, true
        else
            input, false

let private rule1 (input: string) : string * bool =
    let pattern = @"^[aeiou]+|xr|yt"
    let regexMatches = Regex.Matches(input, pattern, RegexOptions.IgnoreCase)

    if regexMatches.Count > 0 then
        input + "ay", true
    else
        input, false

let private rule2 (input: string) : string =
    let pattern = @"^[bcdfghjklmnpqrstvwxyz]+"
    let regexMatches = Regex.Matches(input, pattern, RegexOptions.IgnoreCase)

    if regexMatches.Count > 0 then
        let firstMatch = regexMatches.[0]
        input.Substring(firstMatch.Value.Length) + firstMatch.Value + "ay"
    else
        input

let private rule3 (input: string) : string =
    let pattern = @"^([bcdfghjklmnpqrstvwxyz]*qu)"
    let regexMatches = Regex.Matches(input, pattern, RegexOptions.IgnoreCase)

    if regexMatches.Count > 0 then
        let firstMatch = regexMatches.[0]

        input.Substring(firstMatch.Groups.[0].Length)
        + firstMatch.Groups.[0].Value
        + "ay"
    else
        input

let private rule4 (input: string) : string =
    let pattern = @"^([bcdfghjklmnpqrstvwxyz]+)y"
    let regexMatches = Regex.Matches(input, pattern, RegexOptions.IgnoreCase)

    if regexMatches.Count > 0 then
        let firstMatch = regexMatches.[0]

        input.Substring(firstMatch.Groups.[1].Length)
        + firstMatch.Groups.[1].Value
        + "ay"
    else
        input

let translate (input: string) : string =
    input.Split(' ')
    |> Array.map (fun s ->
        s
        |> rule1
        |> applyIfNotChanged rule3
        |> applyIfNotChanged rule4
        |> applyIfNotChanged rule2
        |> fst)
    |> String.concat " "

printfn "%A" (translate "thrush")
