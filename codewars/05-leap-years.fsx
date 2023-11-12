module LeapYearKata

let isLeapYear (year: int) : bool =
    let divisibleByFour = year % 4 = 0
    let divisibleByHundred = year % 100 = 0
    let divisibleByFourHundred = year % 400 = 0

    divisibleByFour && (not divisibleByHundred) || divisibleByFourHundred

isLeapYear 2020
isLeapYear 2000
isLeapYear 2015
isLeapYear 2100
