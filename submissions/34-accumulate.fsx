module Accumulate

// Helper link:
// https://blog.ploeh.dk/2015/12/22/tail-recurse/

// ================================================

// Slow because appending to the tail of a (linked) list is inefficient. In
// theory, this implementation would never result in a stack overflow, but the
// question is whether anyone has the patience to wait for this function to
// finish running.

let accumulateUsingAppend (func: 'a -> 'b) (input: 'a list) : 'b list =
    let rec applyFn (acc: 'b list) (input': 'a list) =
        match input' with
        | head :: tail -> applyFn (acc @ [ func head ]) tail
        | [] -> acc

    applyFn [] input

// Takes a long time to finish:
accumulateUsingAppend id [ 1..100000 ]

// ================================================

// This function conses unto the accumulator (func head :: acc) instead of
// appending to the accumulator.

let accumulateUsingRev (func: 'a -> 'b) (input: 'a list) : 'b list =
    let rec applyFn (acc: 'b list) (input': 'a list) =
        match input' with
        | head :: tail -> applyFn (func head :: acc) tail
        | [] -> acc

    applyFn [] input |> List.rev

// Way faster approach:
accumulateUsingRev id [ 1..100000 ]

// ===============================================

// Doesn't work!

// let cons x xs = x :: xs

// let accumulatorUsingCons (func: 'a -> 'b) (input: 'a list) : 'b list =
//     let rec applyFn (acc: 'b list) (input': 'a list) : 'b list =
//         match input' with
//         | head::tail -> applyFn (acc << (cons (func head))) tail
//         | [] -> acc

//     applyFn [] input

// =================================================
let accumulate = accumulateUsingRev

accumulate (fun x -> x + 1) List.empty
accumulate id [ 1; 2; 3 ]
accumulate (fun (x: string) -> x.ToUpper()) [ "hello"; "world" ]
accumulate (fun (x: string) -> String.concat " " (accumulate (fun y -> x + y) [ "1"; "2"; "3" ])) [ "a"; "b"; "c" ]
