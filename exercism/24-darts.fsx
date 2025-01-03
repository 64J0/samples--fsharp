module Darts

open System

type Coordinates = double * double

let distance (c: Coordinates) : double =
    let a2 = (fst c) ** 2
    let b2 = (snd c) ** 2

    Math.Sqrt(a2 + b2)

let score (x: double) (y: double) : int =
    let d = distance (x, y)

    match d with
    | x when x <= 10.0 && x > 5.0 -> 1
    | x when x <= 5.0 && x > 1.0 -> 5
    | x when x <= 1.0 -> 10
    | _ -> 0

score (-5.0) (0.0)
score (0.0) (-1.0)
