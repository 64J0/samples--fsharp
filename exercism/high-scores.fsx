let scores (values: int list) : int list = values

let latest (values: int list) : int = values |> List.rev |> List.head

let personalBest (values: int list) : int = values |> List.max

let personalTopThree (values: int list) : int list =
    let bestValues = values |> List.sort |> List.rev
    bestValues.[0..2]
