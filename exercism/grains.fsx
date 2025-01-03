let square (n: int) : Result<uint64, string> =
    if n < 1 || n > 64 then
        Error "square must be between 1 and 64"
    else
        let expoent = n - 1
        float 2 ** expoent |> uint64 |> Ok

let total: Result<uint64, string> =
    seq { 1..64 }
    |> Seq.map square
    |> Seq.map (fun x ->
        match x with
        | Ok n -> n
        | Error _ -> 0UL)
    |> Seq.sum
    |> Ok

total
