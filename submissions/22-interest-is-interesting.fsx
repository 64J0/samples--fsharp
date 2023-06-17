module InterestIsInteresting

let interestRate (balance: decimal) : single =
    match (balance) with
    | x when x < 0.0m -> 3.213f
    | x when x < 1000.0m -> 0.5f
    | x when x < 5000.0m -> 1.621f
    | _ -> 2.475f

interestRate 200.75m

let interest (balance: decimal) : decimal =
    let rate = decimal (interestRate balance)
    rate * balance / 100.0m

interest 200.75m

let annualBalanceUpdate (balance: decimal) : decimal = balance + (interest balance)

annualBalanceUpdate -152964.231M

annualBalanceUpdate 200.75m

let amountToDonate (balance: decimal) (taxFreePercentage: float) : int =
    if (balance < 0.0m) then
        0
    else
        (float balance) * taxFreePercentage * 2.0 / 100.0 |> int

amountToDonate (550.05m) (2.5)
