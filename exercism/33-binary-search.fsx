module BinarySearch =

    let find (input: int array) (value: int) : int option =
        let sortedInput = input |> Array.sort
        let getMidIdx (len: int) = len / 2

        let rec find' (sortedInput: int array) (idxAcc: int) =
            printfn "%A" sortedInput
            let len = sortedInput.Length
            let midIdx = getMidIdx len

            match len with
            | 0 -> None
            | _ ->
                let inputEl = sortedInput.[midIdx]

                if inputEl = value then
                    Some(idxAcc + midIdx)
                elif inputEl > value then
                    let sortedInput' = sortedInput.[.. (midIdx - 1)]
                    find' sortedInput' idxAcc
                else
                    let sortedInput' = sortedInput.[(midIdx + 1) ..]
                    find' sortedInput' (idxAcc + midIdx + 1)

        find' sortedInput 0

    printfn "%A" (find [| 6 |] 6)
    printfn "%A" (find [| 1; 3; 4; 6; 8; 9; 11 |] 7)
    printfn "%A" (find [| 1; 3; 4; 6; 8; 9; 11 |] 1)
    printfn "%A" (find [| 1; 3; 4; 6; 8; 9; 11 |] 11)
