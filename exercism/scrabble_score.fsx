let score word =
    word
    |> string
    |> _.ToUpper()
    |> _.ToCharArray()
    |> Array.map (fun c ->
        match c with
        | 'A'
        | 'E'
        | 'I'
        | 'O'
        | 'U'
        | 'L'
        | 'N'
        | 'R'
        | 'S'
        | 'T' -> 1
        | 'D'
        | 'G' -> 2
        | 'B'
        | 'C'
        | 'M'
        | 'P' -> 3
        | 'F'
        | 'H'
        | 'V'
        | 'W'
        | 'Y' -> 4
        | 'K' -> 5
        | 'J'
        | 'X' -> 8
        | 'Q'
        | 'Z' -> 10
        | _ -> 0)
    |> Array.sum
