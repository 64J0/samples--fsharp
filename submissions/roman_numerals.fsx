let transform (str: string) =
    match str with
    | "" -> "" // for the case of 0
    | "0" -> "" // for the case of 0
    | "1" -> "I"
    | "2" -> "II"
    | "3" -> "III"
    | "4" -> "IV"
    | "5" -> "V"
    | "6" -> "VI"
    | "7" -> "VII"
    | "8" -> "VIII"
    | "9" -> "IX"
    | "I" -> "X"
    | "II" -> "XX"
    | "III" -> "XXX"
    | "IV" -> "XL"
    | "V" -> "L"
    | "VI" -> "LX"
    | "VII" -> "LXX"
    | "VIII" -> "LXXX"
    | "IX" -> "XC"
    | "X" -> "C"
    | "XX" -> "CC"
    | "XXX" -> "CCC"
    | "XL" -> "CD"
    | "L" -> "D"
    | "LX" -> "DC"
    | "LXX" -> "DCC"
    | "LXXX" -> "DCCC"
    | "XC" -> "CM"
    | "C" -> "M"
    | "CC" -> "MM"
    | "CCC" -> "MMM"
    | _ -> failwith "Number out of possible values"

let roman (arabicNumeral: int) : string =
    arabicNumeral
    |> string
    |> _.ToCharArray()
    |> Array.map string
    // |> Array.map (fun s ->
    //     printfn "%A" s
    //     s)
    |> Array.rev
    |> Array.mapi (fun i s ->
        let mutable count = i
        let mutable s2 = transform s

        while count > 0 do
            s2 <- transform s2
            count <- count - 1

        s2)
    |> Array.filter (System.String.IsNullOrEmpty >> not)
    |> Array.rev
    |> Array.fold (+) ""

printfn "%A" (roman 3000)
