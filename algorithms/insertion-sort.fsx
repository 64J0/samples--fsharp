module InsertionSort

// Recursive implementation of the insertion sort algorithm in F#

let rec insertionSort (arrIn: int array) (arrOut: int array): int array =
      match arrIn with
      | [||] -> arrOut
      | _ ->
          let el = Array.head arrIn
          let newArrIn = Array.tail arrIn
          let idxBiggerNum = arrOut |> Array.tryFindIndex (fun x -> x >= el)
          let newArrOut =
              match idxBiggerNum with
              | Some i ->
                  Array.concat [ arrOut.[0 .. (i-1)]
                                 [| el |]
                                 arrOut.[i ..] ]
              | None ->
                  Array.concat [ arrOut
                                 [| el |] ]

          insertionSort (newArrIn) (newArrOut)

let arrIn = [| 5; 2; 4; 6; 1; 3 |]
let arrOut = [||]
insertionSort arrIn arrOut


let arrIn' = [| 5; 2; 4; 6; 1; 3; 3; 3; 10 |]
insertionSort arrIn' arrOut
