open System

type Week =
    | First
    | Second
    | Third
    | Fourth
    | Last
    | Teenth

let meetup (year: int) (month: int) (week: Week) (dayOfWeek: DayOfWeek) : DateTime =
    let rec findWeeksDayOfWeekIncremental (dayOfWeek: DayOfWeek) (dt: DateTime) =
        match dt.DayOfWeek with
        | d when d = dayOfWeek -> dt
        | _ -> findWeeksDayOfWeekIncremental dayOfWeek (dt.AddDays(1))

    let rec findWeeksDayOfWeekDecremental (dayOfWeek: DayOfWeek) (dt: DateTime) =
        match dt.DayOfWeek with
        | d when d = dayOfWeek -> dt
        | _ -> findWeeksDayOfWeekDecremental dayOfWeek (dt.AddDays(-1))

    match week with
    | First ->
        let dt = new DateTime(year, month, 1)
        findWeeksDayOfWeekIncremental dayOfWeek dt
    | Second ->
        let dt = new DateTime(year, month, 1)
        findWeeksDayOfWeekIncremental dayOfWeek (dt.AddDays(7))
    | Third ->
        let dt = new DateTime(year, month, 1)
        findWeeksDayOfWeekIncremental dayOfWeek (dt.AddDays(14))
    | Fourth ->
        let dt = new DateTime(year, month, 1)
        findWeeksDayOfWeekIncremental dayOfWeek (dt.AddDays(21))
    | Last ->
        let dt =
            if month <> 12 then
                new DateTime(year, month + 1, 1)
            else
                new DateTime(year + 1, 1, 1)

        findWeeksDayOfWeekDecremental dayOfWeek (dt.AddDays(-1))
    | Teenth ->
        let dt = new DateTime(year, month, 1)
        findWeeksDayOfWeekIncremental dayOfWeek (dt.AddDays(12))
