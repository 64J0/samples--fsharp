/// Helper function to configure the clock output with the right amount of 0s
let private configureClock (value: int) : string =
    match (string value).Length = 1 with
    | true -> sprintf "0%i" value
    | false -> string value

let private fromHoursToMinutes (hours: int) : int = hours * 60

let create (hours: int) (minutes: int) : int * int =
    let totalMinutes = (fromHoursToMinutes hours) + minutes

    let clockHours = int (float totalMinutes / 60.) |> fun h -> h % 24

    let clockMinutes = totalMinutes % 60

    if totalMinutes >= 0 then
        (clockHours, clockMinutes)
    elif (clockMinutes <> 0) then
        (23 + clockHours, 60 + clockMinutes)
    else
        (24 + clockHours, clockMinutes)

let add (minutes: int) (clock: int * int) : int * int =
    let (clockHours, clockMinutes) = clock
    create (clockHours) (clockMinutes + minutes)

let subtract (minutes: int) (clock: int * int) : int * int =
    let (clockHours, clockMinutes) = clock
    create (clockHours) (clockMinutes - minutes)

let display (clock: int * int) : string =
    sprintf "%s:%s" (configureClock (fst clock)) (configureClock (snd clock))

display (create -1 15) |> printfn "%A"
