let multipleLyrics (currentBottles: string) (nextBottles: string) : string =
    $"""{currentBottles} green bottles hanging on the wall,
{currentBottles} green bottles hanging on the wall,
And if one green bottle should accidentally fall,
There'll be {nextBottles} green bottles hanging on the wall."""

let twoBottlesLyrics (currentBottles: string) (nextBottles: string) : string =
    $"""{currentBottles} green bottles hanging on the wall,
{currentBottles} green bottles hanging on the wall,
And if one green bottle should accidentally fall,
There'll be {nextBottles} green bottle hanging on the wall."""

let oneBottleLyrics (currentBottles: string) (nextBottles: string) : string =
    $"""{currentBottles} green bottle hanging on the wall,
{currentBottles} green bottle hanging on the wall,
And if one green bottle should accidentally fall,
There'll be {nextBottles} green bottles hanging on the wall."""

let rec recite startBottles takeDown : string list =
    match startBottles < takeDown with
    | true -> failwith "There is less startBottles than takeDown"
    | false -> ()

    let (currentBottles, nextBottles) =
        match startBottles with
        | 10 -> ("Ten", "nine")
        | 9 -> ("Nine", "eight")
        | 8 -> ("Eight", "seven")
        | 7 -> ("Seven", "six")
        | 6 -> ("Six", "five")
        | 5 -> ("Five", "four")
        | 4 -> ("Four", "three")
        | 3 -> ("Three", "two")
        | 2 -> ("Two", "one")
        | 1 -> ("One", "no")
        | _ -> failwith $"Invalid startBottles: {startBottles}"

    match startBottles with
    | n when n > 2 ->
        let currentLyrics = multipleLyrics (currentBottles) (nextBottles)
        let currentLyrics = currentLyrics.Split('\n') |> Array.toList

        if takeDown > 1 then
            seq {
                yield currentLyrics
                yield [ "" ]
                yield (recite (startBottles - 1) (takeDown - 1))
            }
            |> List.concat
        else
            currentLyrics
    | n when n = 2 ->
        let currentLyrics = twoBottlesLyrics (currentBottles) (nextBottles)
        let currentLyrics = currentLyrics.Split('\n') |> Array.toList

        if takeDown > 1 then
            seq {
                yield currentLyrics
                yield [ "" ]
                yield (recite (startBottles - 1) (takeDown - 1))
            }
            |> List.concat
        else
            currentLyrics
    | n when n = 1 ->
        let currentLyrics = oneBottleLyrics (currentBottles) (nextBottles)
        let currentLyrics = currentLyrics.Split('\n') |> Array.toList

        if takeDown > 1 then
            seq {
                yield currentLyrics
                yield [ "" ]
                yield (recite (startBottles - 1) (takeDown - 1))
            }
            |> List.concat
        else
            currentLyrics
    | _ -> failwith $"Invalid startBottles: {startBottles}"
