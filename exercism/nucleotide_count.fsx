let nucleotideCounts (strand: string) : Option<Map<char, int>> =
    let initialMap: Option<Map<char, int>> =
        [ ('A', 0); ('C', 0); ('G', 0); ('T', 0) ] |> Map |> Some

    strand
    |> _.ToCharArray()
    |> Array.fold
        (fun acc el ->
            match acc with
            | Some a when Option.isSome (a.TryFind el) ->
                let prevCounter = a.Item el
                a.Add(el, prevCounter + 1) |> Some
            | Some _ -> None
            | None -> None)
        initialMap
