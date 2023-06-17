module DifferenceOfSquares

let squareOfSum (number: int) : int =
    [ 1..number ] |> List.sum |> (fun x -> x * x)

let sumOfSquares (number: int) : int =
    [ 1..number ] |> List.map (fun x -> x * x) |> List.sum

let differenceOfSquares (number: int) : int =
    (squareOfSum number) - (sumOfSquares number)

differenceOfSquares 10
