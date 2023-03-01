module LogLevels

open System

let messageParts (logLine: string): string * string =
    match logLine.Split(":") with
    | [| logLevel; message |] ->
        (logLevel.Trim(), message.Trim())
    | _ -> failwith "Error parsing logLine"

let message (logLine: string): string =
    snd (messageParts logLine)

let logLevel (logLine: string): string =
    (fst (messageParts logLine))
    |> String.filter Char.IsLetter
    |> String.map Char.ToLower

let reformat (logLine: string): string =
    sprintf "%s (%s)" (message logLine) (logLevel logLine)

// Manual tests...
//
// let logLine = "[ERROR]: Invalid operation"
// message("[WARNING]:  Disk almost full\r\n")
// logLevel(logLine)
// reformat(logLine)
