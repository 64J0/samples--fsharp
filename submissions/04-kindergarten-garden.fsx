module KindergartenGarden

open System

type Children =
    | Alice
    | Bob
    | Charlie
    | David
    | Eve
    | Fred
    | Ginny
    | Harriet
    | Ileana
    | Joseph
    | Kincaid
    | Larry

type Plant =
    | Violets
    | Radishes
    | Clover
    | Grass

let studentDisposition = [ Alice; Bob; Charlie; David; Eve; Fred;
                           Ginny; Harriet; Ileana; Joseph; Kincaid;
                           Larry ]

let getStudentIndex (student: string): int =
    studentDisposition
    |> List.findIndex (fun s -> s.ToString() = student)

let getStudentPlants (plantsDisposition: string) (student: string): list<Plant> =
    let startIdx = getStudentIndex student
    let separator = '\n'
    
    plantsDisposition.Split [| separator |]
    |> Seq.collect (Seq.skip (startIdx * 2) >> Seq.take 2)
    |> Seq.map (Char.ToUpper) // Make sure it's always uppercase
    |> Seq.map (fun c ->
                match c with
                | 'V' -> Violets
                | 'R' -> Radishes
                | 'C' -> Clover
                | 'G' -> Grass
                | _ -> failwith "Plant out of possibilities")
    |> Seq.toList

let plants (diagram: string) (student: string): list<Plant> =
    getStudentPlants diagram student
