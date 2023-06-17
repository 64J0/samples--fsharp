module CollatzConjecture

let (|Even|Odd|) number = if number % 2 = 0 then Even else Odd

let rec compute (number: int) (counter: int) : int option =
    match number with
    | x when (x < 1) -> None
    | 1 -> Some counter
    | Even -> compute (number / 2) (counter + 1)
    | Odd -> compute (number * 3 + 1) (counter + 1)

let steps (number: int) : int option = compute number 0

steps 12
