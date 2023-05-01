module MergeSort

// TODO: refac to avoid mutable values

let merge (A: int array) (p: int) (q: int) (r: int): unit =
    let nL = q - p + 1
    let nR = r - q
    let L = A.[ p .. (p + nL - 1)]
    let R = A.[ (q + 1) .. (q + 1 + nR - 1) ]

    let mutable i = 0
    let mutable j = 0
    let mutable k = p

    while (i < nL && j < nR) do
        if (L.[i] <= R.[j]) then
            A.[k] <- L[i]
            i <- i + 1
        else
            A.[k] <- R.[j]
            j <- j + 1
        k <- k + 1

    // Having gone through one of L and R entirely, copy the remainder of the
    // other to the end of A[p:r].
    while (i < nL) do
        A.[k] <- L.[i]
        i <- i + 1
        k <- k + 1
    while (j < nR) do
        A.[k] <- R.[j]
        j <- j + 1
        k <- k + 1

// let A = [| 2; 4; 6; 7; 1; 2; 3; 5 |]
// let p = 0
// let q = 3
// let r = 7

// merge (A) (p) (q) (r)

let rec mergeSort (A: int array) (p: int) (r: int): unit =
    if (p >= r) then ()
    else
        let q = (p + r) / 2 // already get the floor value
        mergeSort (A) (p) (q)
        mergeSort (A) (q + 1) (r)
        merge (A) (p) (q) (r)

let A = [| 15; 6; 7; 10; 13; 2; 4; 6; 7; 1; 2; 3; 5 |]
let p = 0
let r = (A.Length - 1)
mergeSort (A) (p) (r)
A // value mutated x.x
