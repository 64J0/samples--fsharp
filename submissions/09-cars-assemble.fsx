module CarsAssemble

let carsPerHour = 221

let successRate (speed: int): float =
    match speed with
    | s when (s < 1) -> 0.0
    | s when (s <= 4) -> 1.0
    | s when (s <= 8) -> 0.9
    | 9 -> 0.8
    | 10 -> 0.77
    | _ -> failwith "Speed out of acceptable range"

let productionRatePerHour (speed: int): float =
    successRate speed
    |> (fun r -> r * (float speed) * (float carsPerHour))

let workingItemsPerMinute (speed: int): int =
    productionRatePerHour speed
    |> (fun p -> p / 60.0)
    |> int

workingItemsPerMinute 1
