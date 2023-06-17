module ReverseString

let reverse (input: string) : string =
    input |> Seq.rev |> Seq.map string |> String.concat ""

reverse "cool"
reverse ""
