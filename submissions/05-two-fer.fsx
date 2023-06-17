module TwoFer

let twoFer (input: string option) : string =
    let name = Option.defaultValue "you" input
    sprintf "One for %s, one for me." name
