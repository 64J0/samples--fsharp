open System

let readStdin () : int32 = Console.In.ReadLine() |> int

// Soma de progressão aritmética
let getMultiplesSum (n: int) (multiple: int) : uint64 =
    let qnt = ((n - 1) / multiple) |> uint64
    (qnt + 1UL) * (uint64 multiple) * qnt / 2UL

let main () =
    let t = readStdin ()

    for _ in 1..t do
        let n = readStdin ()

        let multiplesSum =
            (getMultiplesSum n 3) + (getMultiplesSum n 5) - (getMultiplesSum n 15)

        Console.Out.WriteLine multiplesSum

    0

main ()
