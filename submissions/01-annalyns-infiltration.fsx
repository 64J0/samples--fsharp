module AnnalynsInfiltration

let canFastAttack (knightIsAwake: bool) = not knightIsAwake

let canSpy (knightIsAwake: bool) (archerIsAwake: bool) (prisonerIsAwake: bool) =
    knightIsAwake || prisonerIsAwake || archerIsAwake

let canSignalPrisoner (archerIsAwake: bool) (prisonerIsAwake: bool) = (not archerIsAwake) && prisonerIsAwake

let canFreePrisoner (knightIsAwake: bool) (archerIsAwake: bool) (prisonerIsAwake: bool) (petDogIsPresent: bool) =
    ((not (knightIsAwake || archerIsAwake)) && prisonerIsAwake)
    || ((not archerIsAwake) && petDogIsPresent)
