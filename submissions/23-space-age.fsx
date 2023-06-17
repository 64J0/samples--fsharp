module SpaceAge

type Planet =
    | Mercury
    | Venus
    | Earth
    | Mars
    | Jupiter
    | Saturn
    | Uranus
    | Neptune

let age (planet: Planet) (seconds: int64) : float =
    let oneYear = 31_557_600 // 365.25 Earth days
    let earthYears = (float seconds) / (float oneYear)

    match planet with
    | Mercury -> earthYears / 0.2408467
    | Venus -> earthYears / 0.61519726
    | Earth -> earthYears
    | Mars -> earthYears / 1.8808158
    | Jupiter -> earthYears / 11.862615
    | Saturn -> earthYears / 29.447498
    | Uranus -> earthYears / 84.016846
    | Neptune -> earthYears / 164.79132

age Earth 1_000_000_000
