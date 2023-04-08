module PerfectNumbers

type Classification =
    | Perfect
    | Abundant
    | Deficient 

let rec getAliquotSum (number: int) (state: int) (out: int list): int =
    if (state = number) then out |> List.sum
    else
        match (number % state) with
        | 0 -> getAliquotSum number (state + 1) (state::out)
        | _ -> getAliquotSum number (state + 1) out

let classify (number: int): Classification option =
    if (number < 1) then None
    else
        let aliquotSum = getAliquotSum number 2 [1]

        if (aliquotSum = number) then Some Classification.Perfect
        elif (aliquotSum > number) then Some Classification.Abundant
        else Some Classification.Deficient

// Perfect:
classify 6
classify 28
// Abundant:
classify 12
classify 24
// Deficient:
classify 8
classify 13
// Wrong input:
classify 0
classify -1
