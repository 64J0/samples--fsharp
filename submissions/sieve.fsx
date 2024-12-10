let primes (limit: int) =
    let rec primes (remainingNumbers: int list) (ans: int list) =
        printfn "%A; %A" remainingNumbers ans

        match remainingNumbers with
        | h :: _xs ->
            let filteredNumbers = remainingNumbers |> List.filter (fun x -> x % h <> 0)
            let newAns = h :: ans
            primes filteredNumbers newAns
        | [] -> ans

    if limit < 2 then
        []
    else
        primes ([ 2..limit ]) ([]) |> List.rev

printfn "%A" (primes 10)
