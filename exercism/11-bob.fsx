module Bob

// Using Active Patterns
// https://learn.microsoft.com/en-us/dotnet/fsharp/language-reference/active-patterns

let (|Silence|_|) (phrase: string) : unit option =
    if System.String.IsNullOrWhiteSpace(phrase) then
        Some()
    else
        None

let (|Yell|_|) (phrase: string) : unit option =
    if phrase <> phrase.ToLower() && phrase = phrase.ToUpper() then
        Some()
    else
        None

let (|Question|_|) (phrase: string) : unit option =
    if phrase.TrimEnd().EndsWith("?") then Some() else None

let response (phrase: string) : string =
    match phrase with
    | Silence -> "Fine. Be that way!"
    | Yell & Question -> "Calm down, I know what I'm doing!"
    | Yell -> "Whoa, chill out!"
    | Question -> "Sure."
    | _ -> "Whatever."

response "1, 2, 3"
