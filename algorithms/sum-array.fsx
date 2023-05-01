module SumArray

let rec sumArray (arrIn: int array) (result: int): int =
    match arrIn with
    | [||] -> result
    | _ ->
        let result' = (Array.head arrIn) + result
        let arrIn' = Array.tail arrIn
        sumArray arrIn' result'

sumArray [| 1 .. 10 |] 0

// Initialization:
// arrIn = [| 1 |] -> result = 1
//
// Maintenance:
// arrIn = [| x1, x2, ..., xn |] -> result = arrIn.[0] + arrIn.[1] + ... +
// arrIn.[n]
