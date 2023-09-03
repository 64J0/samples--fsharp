module RobotSimulator

type Direction =
    | North
    | East
    | South
    | West

type Position = int * int
type RobotOrientation = Direction * Position

let create (direction: Direction) (position: Position) : RobotOrientation = (direction, position)

let turnLeft (robot: RobotOrientation) : RobotOrientation =
    let direction, position = robot

    match direction with
    | Direction.North -> (Direction.West, position)
    | Direction.West -> (Direction.South, position)
    | Direction.South -> (Direction.East, position)
    | Direction.East -> (Direction.North, position)

let turnRight (robot: RobotOrientation) : RobotOrientation =
    let direction, position = robot

    match direction with
    | Direction.North -> (Direction.East, position)
    | Direction.West -> (Direction.North, position)
    | Direction.South -> (Direction.West, position)
    | Direction.East -> (Direction.South, position)

let advance (robot: RobotOrientation) : RobotOrientation =
    let direction, position = robot
    let xCoord, yCoord = position

    match direction with
    | Direction.North -> (Direction.North, (xCoord, yCoord + 1))
    | Direction.West -> (Direction.West, (xCoord - 1, yCoord))
    | Direction.South -> (Direction.South, (xCoord, yCoord - 1))
    | Direction.East -> (Direction.East, (xCoord + 1, yCoord))

let rec recursiveMove (nextInstruction: string) (currentState: RobotOrientation) =
    match nextInstruction with
    | "" -> currentState
    | _ ->
        let head = nextInstruction.[0].ToString()
        let tail = nextInstruction.[1..].ToString()

        let nextState =
            match head with
            | "R" -> turnRight currentState
            | "L" -> turnLeft currentState
            | "A" -> advance currentState
            | _ -> failwith "Invalid instruction"

        recursiveMove tail nextState

let move (instructions: string) (robot: RobotOrientation) : RobotOrientation = recursiveMove instructions robot
