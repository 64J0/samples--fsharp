module TisburyTreasureHunt

let getCoordinate (line: string * string) : string = snd line

let convertCoordinate (coordinate: string) : int * char =

    (coordinate.[0].ToString() |> int, char coordinate.[1])

let compareRecords (azarasData: string * string) (ruisData: string * (int * char) * string) : bool =
    let azarasCoord = (snd azarasData) |> convertCoordinate
    let (_, ruisCoord, _) = ruisData
    azarasCoord = ruisCoord

let createRecord
    (azarasData: string * string)
    (ruisData: string * (int * char) * string)
    : (string * string * string * string) =
    if compareRecords azarasData ruisData then
        let (location, _coordinates, quadrant) = ruisData
        (snd azarasData, location, quadrant, fst azarasData)
    else
        ("", "", "", "")
