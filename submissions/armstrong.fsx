let isArmstrongNumber (number: int) : bool =
    let charArrayNumber = number |> string |> _.ToCharArray()

    charArrayNumber
    |> Array.map (string >> float)
    |> Array.fold (fun acc x -> (x ** charArrayNumber.Length) + acc) (0.)
    |> int
    |> function
        | n when n = number -> true
        | _ -> false

printfn "%A" (isArmstrongNumber 153)
