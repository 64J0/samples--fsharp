let value (colors: string list) : int =
    let getNumber (color: string) =
        match color with
        | "black" -> "0"
        | "brown" -> "1"
        | "red" -> "2"
        | "orange" -> "3"
        | "yellow" -> "4"
        | "green" -> "5"
        | "blue" -> "6"
        | "violet" -> "7"
        | "grey" -> "8"
        | "white" -> "9"
        | _ -> failwith "Invalid color!"

    colors |> List.take 2 |> List.map (getNumber) |> List.fold (+) "" |> int

value [ "brown"; "black" ] |> printfn "%A"
