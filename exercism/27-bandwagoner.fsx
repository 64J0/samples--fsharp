module Bandwagoner

type Coach = { Name: string; FormerPlayer: bool }
type Stats = { Wins: int; Losses: int }

type Team =
    { Name: string
      Coach: Coach
      Stats: Stats }

let createCoach (coachName: string) (formerPlayer: bool) : Coach =
    { Name = coachName
      FormerPlayer = formerPlayer }

// createCoach "Larry Bird" true

let createStats (numberOfWins: int) (numberOfLosses: int) : Stats =
    { Wins = numberOfWins
      Losses = numberOfLosses }

// createStats 58 24

let createTeam (teamName: string) (coach: Coach) (stats: Stats) : Team =
    { Name = teamName
      Coach = coach
      Stats = stats }

// let coach = createCoach "Larry Bird" true
// let record = createStats 58 24
// createTeam "Indiana Pacers" coach record

let replaceCoach (team: Team) (newCoach: Coach) : Team = { team with Coach = newCoach }

// let coach = createCoach "Larry Bird" true
// let record = createStats 58 24
// let team = createTeam "Indiana Pacers" coach record

// let newCoach = createCoach "Isiah Thomas" true
// replaceCoach team newCoach

let isSameTeam (teamLeft: Team) (teamRight: Team) : bool = teamLeft = teamRight

// let pacersCoach = createCoach "Larry Bird" true
// let pacersStats = createStats 58 24
// let pacersTeam = createTeam "Indiana Pacers" pacersCoach pacersStats

// let lakersCoach = createCoach "Del Harris" false
// let lakersStats = createStats 61 21
// let lakersTeam = createTeam "LA Lakers" lakersCoach lakersStats

// isSameTeam pacersTeam lakersTeam

// ===============================================

// The coach's name is "Gregg Popovich"
// The coach is a former player
// The team's name is the "Chicago Bulls"
// The team has won 60 or more games
// The team has more losses than wins

let rootForTeam (team: Team) : bool =
    let { Name = name
          Coach = coach
          Stats = stats } =
        team

    let coachVerification =
        match coach with
        | { Name = "Gregg Popovich" } -> true
        | { FormerPlayer = true } -> true
        | _ -> false

    let statsVerification =
        match stats with
        | { Wins = w } when w >= 60 -> true
        | { Wins = w; Losses = l } when l > w -> true
        | _ -> false

    let teamVerification = (name = "Chicago Bulls")

    coachVerification || statsVerification || teamVerification
