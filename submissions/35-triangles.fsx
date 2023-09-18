module Triangle

// Determine if a triangle is equilateral, isosceles, or scalene.
//
// An equilateral triangle has all three sides the same length.
//
// An isosceles triangle has at least two sides the same length. (It is
// sometimes specified as having exactly two sides the same length, but for the
// purposes of this exercise we'll say at least two.)
//
// A scalene triangle has all sides of different lengths.

let validTriangle (triangle: float list) =
    let validInput = (List.length triangle) = 3

    let invalidMeasures = triangle |> List.exists (fun m -> m <= 0)

    match validInput && (not invalidMeasures) with
    | false -> (false, 0.0, 0.0, 0.0)
    | true ->
        let a = triangle |> List.item 0
        let b = triangle |> List.item 1
        let c = triangle |> List.item 2

        let validSizes = (a + b >= c) && (b + c >= a) && (a + c >= b)

        (validSizes, a, b, c)

let equilateral (triangle: float list) =
    match validTriangle triangle with
    | (false, _, _, _) -> false
    | (true, a, b, c) -> (a = b) && (b = c)

let isosceles (triangle: float list) =
    match validTriangle triangle with
    | (false, _, _, _) -> false
    | (true, a, b, c) -> (a = b) || (b = c) || (a = c)

let scalene (triangle: float list) =
    match validTriangle triangle with
    | (false, _, _, _) -> false
    | (true, a, b, c) -> (a <> b) && (b <> c) && (a <> c)


equilateral [ 2.0; 2.0; 2.0 ]

scalene [ 3.0; 4.0; 3.0 ]
