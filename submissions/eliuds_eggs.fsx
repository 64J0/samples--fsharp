let eggCount (n: int) : int =
    sprintf "%B" n
    |> _.ToCharArray()
    |> Array.map (fun c -> if c = '1' then 1 else 0)
    |> Array.sum
