// Inspiration from:
//
// https://www.youtube.com/watch?v=sjqB70KIfog

open System

module LongestConsecutive =

    let longestConsecutive1 (nums: int list) : int =
        nums
        |> List.distinct // remove potential duplicates
        |> List.sort
        |> List.fold
            (fun acc el ->
                let ans, counter, prevEl = acc

                if (el = prevEl + 1) then (ans, counter + 1, el)
                else if ans < counter then (counter, 1, el)
                else (ans, 1, el))
            (1, 1, nums.[0])
        |> function
            | f, _, _ -> f

    let longestConsecutive2 (nums: int list) : int =
        let numsMap =
            nums
            |> List.distinct // remove potential duplicates
            |> List.map (fun el -> el, el)
            |> Map.ofList

        let mutable ans = 1

        for i in [ 0 .. nums.Length - 1 ] do
            // skip if the number is not the first in the sequence
            match numsMap.TryFind(nums.[i] - 1) with
            | Some _ -> ()
            | None ->
                let mutable counter = 1

                while Option.isSome (numsMap.TryFind(nums.[i] + counter)) do
                    counter <- counter + 1

                if ans < counter then ans <- counter else ()

        ans

module TopKFrequent =

    let topKFrequent (nums: int list, k: int) : int list =
        let numsMap =
            nums
            |> List.fold
                (fun (acc: Map<int, int>) (e: int) ->
                    match acc.TryGetValue e with
                    | true, v -> acc.Add(e, v + 1)
                    | false, _ -> acc.Add(e, 1))
                Map.empty<int, int>

        // Does not work that well with functional languages which have immutability
        // let buckets: int array array =
        //     [ 0 .. nums.Length ]
        //     |> List.map (fun _ -> [||])
        //     |> List.toArray

        // let keys = nums |> List.distinct

        // let ans = []

        // keys
        // |> List.fold (fun acc e ->
        //     let idx =
        //         match numsMap.TryGetValue e with
        //         | true, v -> v
        //         | false, _ -> failwith "Impossible"

        //     Array.append acc.[idx] [| e |]
        // ) buckets

        numsMap
        |> Map.toList
        |> List.sortBy snd
        |> List.rev
        |> List.map fst
        |> List.take k

module ReverseOnlyLetters =

    let reverseOnlyLetters (s: string) : string =
        let mutable left = 0
        let mutable right = s.Length - 1
        let mutable ans = s.ToCharArray()

        while left < right do
            if not (Char.IsAsciiLetter ans.[left]) then
                left <- left + 1
            elif not (Char.IsAsciiLetter ans.[right]) then
                right <- right - 1
            else
                let tmp = ans.[right]
                ans.[right] <- ans.[left]
                ans.[left] <- tmp
                left <- left + 1
                right <- right - 1

        String(ans)

let main () : int =
    let input = [ 100; 4; 200; 1; 3; 2 ]
    printfn "longestConsecutive1: %i" (LongestConsecutive.longestConsecutive1 input)

    printfn "\nlongestConsecutive2: %i" (LongestConsecutive.longestConsecutive2 input)

    printfn "\ntopKFrequent: %A" (TopKFrequent.topKFrequent ([ 1; 1; 1; 1; 2; 2; 2; 3; 3; 2; 2; 3; 3; 3; 3; 3; 3 ], 2))

    printfn "\nreverseOnlyLetters: %s" (ReverseOnlyLetters.reverseOnlyLetters "ab-c-d")
    0

main ()
