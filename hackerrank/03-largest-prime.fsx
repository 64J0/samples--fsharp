open System

let readStdin () = Console.ReadLine()

let writeStdOut (res: int64) = Console.WriteLine(string res)

let rec getPrimes (state: int64 list) (primes: int64 list) : int64 list =
    match state with
    | [] -> primes
    | _ ->
        let newPrime = state |> List.head
        let newState = state |> List.filter (fun x -> x % newPrime <> 0L)

        getPrimes newState (newPrime :: primes)

let getLargestPrimeFactor (n: int64) : int64 =
    let max =
        match n % 2L = 0L with
        | true -> n / 2L
        | false -> n

    let primes = getPrimes [ 3L .. 2L .. max ] [ 2L ]

    primes |> List.filter (fun x -> n % x = 0L) |> List.head

let main () =
    let t = readStdin () |> int

    for _ in [ 1..t ] do
        let n = readStdin () |> int64
        let largestPrime = getLargestPrimeFactor n
        writeStdOut largestPrime

main ()
