module Allergies

type Allergen =
    | Eggs
    | Peanuts
    | Shellfish
    | Strawberries
    | Tomatoes
    | Chocolate
    | Pollen
    | Cats

// From https://richiban.uk/2013/07/19/converting-decimal-integers-to-binary-strings-in-different-languages/
let rec intToBinary i =
    match i with
    | 0
    | 1 -> string i
    | _ ->
        let bit = string (i % 2)
        (intToBinary (i / 2)) + bit

let allergenList =
    [ Eggs; Peanuts; Shellfish; Strawberries; Tomatoes; Chocolate; Pollen; Cats ]

let list (codedAllergies: int) =
    intToBinary codedAllergies
    |> Seq.rev
    |> Seq.zip allergenList
    |> Seq.filter (fun a -> (snd a) = '1')
    |> Seq.map fst
    |> Seq.toList

list 34

let allergicTo (codedAllergies: int) (allergen: Allergen) =
    list codedAllergies |> List.exists (fun a -> a = allergen)

allergicTo 34 Chocolate
