let create (position: int * int) : bool =
    let x, y = position
    x >= 0 && x < 8 && y >= 0 && y < 8

let canAttack (queen1: int * int) (queen2: int * int) : bool =
    let checkHorizontal (queen1, queen2) : bool =
        let _, y1 = queen1
        let _, y2 = queen2
        y1 = y2

    let checkVertical (queen1, queen2) : bool =
        let x1, _ = queen1
        let x2, _ = queen2
        x1 = x2

    // The distance in X must be the same distance in Y for them to be at the same
    // diagonal
    let checkDiagonal (queen1, queen2) : bool =
        let x1, y1 = queen1
        let x2, y2 = queen2
        abs (x1 - x2) = abs (y1 - y2)

    checkHorizontal (queen1, queen2)
    || checkVertical (queen1, queen2)
    || checkDiagonal (queen1, queen2)
