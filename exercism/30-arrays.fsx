module BirdWatcher

let lastWeek: int[] = [| 0; 2; 5; 3; 7; 8; 4 |]

let yesterday (counts: int[]) : int = counts |> Array.rev |> Array.item 1

let total (counts: int[]) : int = counts |> Array.sum

let dayWithoutBirds (counts: int[]) : bool = counts |> Array.exists (fun n -> n = 0)

let incrementTodaysCount (counts: int[]) : int[] =
    counts
    |> Array.rev
    |> Array.mapi (fun idx x -> if idx = 0 then x + 1 else x)
    |> Array.rev

let unusualWeek (counts: int[]) : bool =
    let countsWithIdx = counts |> Array.mapi (fun idx x -> (idx, x))

    let evenDayCounts =
        countsWithIdx
        |> Array.filter (fun (idx, _x) -> idx % 2 <> 0)
        |> Array.map (fun (_idx, x) -> x)

    let oddDayCounts =
        countsWithIdx
        |> Array.filter (fun (idx, _x) -> idx % 2 = 0)
        |> Array.map (fun (_idx, x) -> x)

    let noBirdsEvenDays = (evenDayCounts |> Array.sum) = 0
    let tenBirdsEvenDays = evenDayCounts |> Array.forall (fun x -> x = 10)
    let fiveBirdsOddDays = oddDayCounts |> Array.forall (fun x -> x = 5)

    noBirdsEvenDays || tenBirdsEvenDays || fiveBirdsOddDays
