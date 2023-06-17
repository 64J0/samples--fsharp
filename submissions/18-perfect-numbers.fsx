module PerfectNumbers

type Classification =
    | Perfect
    | Abundant
    | Deficient

let factors (x: int) : List<int> =
    [ for i in 1 .. x / 2 do
          if x % i = 0 then
              yield i ]

let classify (n: int) : Option<Classification> =
    let aliquotSum x = List.sum (factors x)

    let classification =
        function
        | sum when sum < n -> Deficient
        | sum when sum > n -> Abundant
        | _ -> Perfect

    Some n
    |> Option.filter (fun x -> x > 0)
    |> Option.map aliquotSum
    |> Option.map classification

// Perfect:
classify 6
classify 28
classify 33550336
// Abundant:
classify 12
classify 24
classify 30
classify 33550335
// Deficient:
classify 8
classify 13
classify 33550337
// Wrong input:
classify 0
classify -1
