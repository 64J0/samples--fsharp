let xbonacci (s: int list) (n: int) : int list =
    let mutable counter = 1
    let mutable result = s
    let sLen = s.Length

    while counter < n do
        let existElement = result |> List.tryItem (counter - 1)

        match existElement with
        | Some _ -> ()
        | None ->
            let subList = result |> List.skip (counter - sLen)

            if subList.Length > 0 then
                result <- result @ [ subList |> List.sum ]
            else
                ()

        counter <- counter + 1

    result

xbonacci [ 0; 1 ] 10
xbonacci [ 1; 1 ] 10
xbonacci [ 0; 0; 0; 0; 1 ] 10
xbonacci [ 1; 2; 3; 5; 7; 9; 11; 13; 17; 19; 23 ] 20
xbonacci [ 1; 2; 3; 4; 5; 6; 7; 8; 9; 0 ] 10
xbonacci [ 1; 2; 3; 4; 5; 6; 7; 8; 9 ] 9
xbonacci [ 1 ] 10
