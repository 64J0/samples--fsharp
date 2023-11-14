open System

let readStdin () : string = Console.ReadLine()

let writeStdout (msg: string) : unit = Console.WriteLine msg

let evenFibonacciSum (n: int64) : int64 =
    if n < 2L then
        0L
    else

        let fibArray = [| 1L; 2L |]
        let mutable nextFibVal = 3L
        let mutable state = 2L

        while nextFibVal < n do
            if nextFibVal % 2L = 0L then
                state <- state + nextFibVal

            nextFibVal <- fibArray.[0] + fibArray.[1]

            fibArray.[0] <- fibArray.[1]
            fibArray.[1] <- nextFibVal

        state

evenFibonacciSum 100L

let main () : int =
    let t = readStdin () |> int

    for _ in [ 1..t ] do
        let n = readStdin () |> int64
        let sum = evenFibonacciSum n
        writeStdout (string sum)

    0

main ()
