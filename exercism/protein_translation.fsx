let proteins (rna: string) =
    rna
    |> Seq.chunkBySize 3
    |> Seq.map (fun c -> Array.map string c)
    |> Seq.map (fun c -> Array.fold (+) "" c)
    |> Seq.map (fun codon ->
        match codon with
        | "AUG" -> "Methionine"
        | "UUU"
        | "UUC" -> "Phenylalanine"
        | "UUA"
        | "UUG" -> "Leucine"
        | "UCU"
        | "UCC"
        | "UCA"
        | "UCG" -> "Serine"
        | "UAU"
        | "UAC" -> "Tyrosine"
        | "UGU"
        | "UGC" -> "Cysteine"
        | "UGG" -> "Tryptophan"
        | "UAA"
        | "UAG"
        | "UGA"
        | _ -> "STOP")
    |> Seq.takeWhile (fun s -> s <> "STOP")
    |> Seq.toList
