module RunLengthEncoding

let encode (input: string) =
    input
    |> Seq.fold (fun acc c ->
                 match acc with
                 | (x, y, z) when (x = string c) -> (x, y + 1, z)
                 | (x, y, z) when (y = 1) -> (string c, 1, $"{z}{x}")
                 | (x, y, z) -> (string c, 1, $"{z}{y}{x}"))
                 ("", 1, "")
    |> function
       | (x, y, z) when (y = 1) -> $"{z}{x}"
       | x, y, z -> $"{z}{y}{x}"

encode "AABCCCDEEEE"
encode ""

let decode (input: string) =
    input
    |> Seq.fold (fun acc c ->
                 match acc with
                 | (x, y) when (System.Char.IsDigit c) -> (int ($"{x}{c}"), y)
                 | (x, y) when (x > 0) -> (0,
                                           [1..x]
                                           |> List.map (fun _ -> string c)
                                           |> List.reduce (fun acc' c' -> $"{acc'}{c'}")
                                           |> sprintf "%s%s" y)
                 | (_, y) -> (0, $"{y}{c}"))
                 (0, "")
    |> snd

decode "2A3B4C"
decode "12WB12W3B24WB"
