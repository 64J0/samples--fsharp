let findNb (n: uint64) : int =
    let rec getN (x: float) (acc: uint64) =
        let xCube = x ** 3.0
        let newAcc = acc + (uint64 xCube)

        if (newAcc > n) then -1
        elif newAcc = n then int x
        else getN (x + 1.0) (newAcc)

    getN 1.0 0UL

findNb (4183059834009UL)
findNb (24723578342962UL)
findNb (135440716410000UL)
findNb (40539911473216UL)
findNb (26825883955641UL)
