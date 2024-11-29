open System

let lowerCaseGreek = [ 'α' .. 'ω' ]

let transform (c: Char) : string =
    match c with
    | '-' -> "_"
    | ' ' -> ""
    | camelCase when Char.IsUpper camelCase -> sprintf "-%c" (Char.ToLower camelCase)
    | numericChar when Char.IsNumber numericChar -> ""
    | lowerGreek when lowerCaseGreek |> List.contains lowerGreek -> "?"
    | otherC -> string otherC

let clean (identifier: string) : string = identifier |> String.collect transform
