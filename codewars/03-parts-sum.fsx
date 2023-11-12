let partsSums (ls: int array) : int array =
    let resultArray = Array.zeroCreate (ls.Length + 1)
    let mutable idx = 0
    let mutable total = ls |> Array.sum

    for _ in ls do
        resultArray.[idx] <- total
        total <- total - ls.[idx]
        idx <- idx + 1

    resultArray

// let rec calculate (idx: int) (acc: int array) =
//     match acc with
//     | [||] -> resultArray
//     | _ ->
//         let tail = acc.[1..]
//         resultArray.[idx] <- acc |> Array.sum
//         calculate (idx + 1) tail

// calculate 0 ls

partsSums [||]
partsSums [| 0; 1; 3; 6; 10 |]
partsSums [| 744125; 935; 407; 454; 430; 90; 144; 6710213; 889; 810; 2579358 |]
