let allDigits =
    [ "    _  _ "
      "  | _| _|"
      "  ||_  _|"
      "         "
      "    _  _ "
      "|_||_ |_ "
      "  | _||_|"
      "         "
      " _  _  _ "
      "  ||_||_|"
      "  ||_| _|"
      "         " ]

let zero = [ " _ "; "| |"; "|_|"; "   " ]
let one = allDigits |> List.take 4 |> List.map (fun line -> line.[0..2])
let two = allDigits |> List.take 4 |> List.map (fun line -> line.[3..5])
let three = allDigits |> List.take 4 |> List.map (fun line -> line.[6..8])
let four = allDigits.[4..7] |> List.map (fun line -> line.[0..2])
let five = allDigits.[4..7] |> List.map (fun line -> line.[3..5])
let six = allDigits.[4..7] |> List.map (fun line -> line.[6..8])
let seven = allDigits.[8..11] |> List.map (fun line -> line.[0..2])
let eight = allDigits.[8..11] |> List.map (fun line -> line.[3..5])
let nine = allDigits.[8..11] |> List.map (fun line -> line.[6..8])

let convert (input: string list) : string option =
    let validLength = input.Length % 4 = 0

    let validDigitLength =
        input |> List.exists (fun (x: string) -> x.Length % 3 <> 0) |> not

    match validLength && validDigitLength with
    | false -> None
    | true ->
        let mutable output: string list = []
        let mutable levelY = 0
        let mutable levelX = 0

        while (levelY * 4) < input.Length do
            if levelY > 0 then
                output <- "," :: output

            let row = input.[(levelY * 4) .. ((levelY * 4) + 3)]

            levelX <- 0

            while (levelX * 3) < row.[0].Length do
                let digit = row |> List.map (fun line -> line.[(levelX * 3) .. ((levelX * 3) + 2)])

                // printfn "%A" digit

                match digit with
                | d when d = zero -> output <- "0" :: output
                | d when d = one -> output <- "1" :: output
                | d when d = two -> output <- "2" :: output
                | d when d = three -> output <- "3" :: output
                | d when d = four -> output <- "4" :: output
                | d when d = five -> output <- "5" :: output
                | d when d = six -> output <- "6" :: output
                | d when d = seven -> output <- "7" :: output
                | d when d = eight -> output <- "8" :: output
                | d when d = nine -> output <- "9" :: output
                | _ -> output <- "?" :: output

                levelX <- levelX + 1

            levelY <- levelY + 1


        output |> List.fold (fun acc x -> $"{x}{acc}") "" |> Some

let rows = [ " _ "; "| |"; "|_|"; "   " ]

convert rows |> printfn "Example 1: %A"

let rows2 =
    [ "       _     _        _  _ "
      "  |  || |  || |  |  || || |"
      "  |  ||_|  ||_|  |  ||_||_|"
      "                           " ]

convert rows2 |> printfn "Example 2: %A"

let rows3 =
    [ "    _  _ "
      "  | _| _|"
      "  ||_  _|"
      "         "
      "    _  _ "
      "|_||_ |_ "
      "  | _||_|"
      "         "
      " _  _  _ "
      "  ||_||_|"
      "  ||_| _|"
      "         " ]

convert rows3 |> printfn "Example 3: %A"
