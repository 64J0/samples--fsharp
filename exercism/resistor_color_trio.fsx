let private allColors =
    [ ("black", 0.)
      ("brown", 1.)
      ("red", 2.)
      ("orange", 3.)
      ("yellow", 4.)
      ("green", 5.)
      ("blue", 6.)
      ("violet", 7.)
      ("grey", 8.)
      ("white", 9.) ]

let private colorCode (color: string) =
    List.find (fun (strColor, _number) -> strColor = color) allColors

let label (colors: string list) : string =
    let (_, firstNumber) = colorCode colors.[0]
    let (_, secondNumber) = colorCode colors.[1]
    let (_, thirdNumber) = colorCode colors.[2]

    let resistanceBase = firstNumber * 10. + secondNumber |> string

    let resistanceAddedZeroes =
        if thirdNumber > 0 then
            [ for _ in 1. .. thirdNumber do
                  "0" ]
            |> List.fold (+) ""
        else
            ""

    let resistanceFinal = resistanceBase + resistanceAddedZeroes

    let quantityOfZeros =
        resistanceFinal
        |> _.ToCharArray()
        |> Array.countBy id
        |> Array.tryFind (fun (x, _qnt) -> x = '0')

    match quantityOfZeros with
    | Some(_, x) when x < 3 -> sprintf "%s ohms" resistanceFinal.[0..2]
    | Some(_, x) when x < 6 -> sprintf "%s kiloohms" (resistanceFinal.Replace("000", ""))
    | Some(_, x) when x < 9 -> sprintf "%s megaohms" (resistanceFinal.Replace("000000", ""))
    | Some(_, x) when x < 12 -> sprintf "%s gigaohms" (resistanceFinal.Replace("000000000", ""))
    | _ -> sprintf "%s ohms" resistanceFinal

printfn "%A" (label [ "red"; "black"; "red" ])
