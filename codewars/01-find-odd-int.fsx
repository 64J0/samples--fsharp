let findOdd (numbers: int list) =
    numbers
    |> List.groupBy id
    |> List.map (fun x -> fst x, (snd x) |> List.length)
    |> List.find (fun x -> (snd x) % 2 = 1)
    |> fst

findOdd [ 1; 1; 2 ]
