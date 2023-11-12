let validate (str: string) =
    let isEven = str.Length % 2 = 0

    str
    |> Seq.filter System.Char.IsDigit
    |> Seq.map (string >> int)
    |> Seq.mapi (fun i d ->
        if isEven && i % 2 = 0 then d * 2
        elif (not isEven) && i % 2 = 1 then d * 2
        else d)
    |> Seq.map (fun d ->
        if d > 9 then
            string d |> Seq.map (string >> int) |> Seq.sum
        else
            d)
    |> Seq.sum
    |> function
        | x when x % 10 = 0 -> true
        | _ -> false

validate ("891")
validate ("677  674")
validate ("8707  3848  8176")
