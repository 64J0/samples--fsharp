let distance (strand1: string) (strand2: string) : int option =
    match strand1.Length = strand2.Length with
    | false -> None
    | true ->
        List.fold2
            (fun acc t1 t2 -> if t1 <> t2 then acc + 1 else acc)
            0
            (strand1.ToCharArray() |> Array.toList)
            (strand2.ToCharArray() |> Array.toList)
        |> Some
