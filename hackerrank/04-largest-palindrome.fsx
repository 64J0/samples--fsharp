let possibleSpace () =
    let reverseNumber (num: int) =
        (string num).ToCharArray() |> Array.rev |> System.String

    let set1 = [| 100..999 |]
    let len = set1.Length - 1

    seq {
        for x in [ 0..len ] do
            for y in [ 0..len ] do
                yield set1.[x], set1.[y]
    }
    |> Seq.map (fun (x, y) -> if x > y then x, y else y, x)
    |> Seq.distinct
    |> Seq.map (fun p -> fst p * snd p)
    |> Seq.filter (fun p -> (string p) = (reverseNumber p)) // palindrome
    |> Seq.sort

let main () : int =
    let space = possibleSpace ()
    let t = System.Console.ReadLine() |> int

    for _ in [ 1..t ] do
        let n = System.Console.ReadLine() |> int
        space |> Seq.filter (fun x -> x < n) |> Seq.last |> System.Console.WriteLine

    0

main ()
