let convert (number: int) : string =
    let mutable ans = ""

    if number % 3 = 0 then
        ans <- $"{ans}Pling"

    if number % 5 = 0 then
        ans <- $"{ans}Plang"

    if number % 7 = 0 then
        ans <- $"{ans}Plong"

    if System.String.IsNullOrEmpty ans then
        string number
    else
        ans
