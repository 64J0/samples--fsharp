module TracksOnTracksOnTracks

let newList: string list = List.empty

let existingList: string list = [ "F#"; "Clojure"; "Haskell" ]

let addLanguage (newLanguage: string) (languageList: string list) : string list = newLanguage :: languageList

let countLanguages (languages: string list) : int = languages |> List.length

let reverseList (languageList: string list) : string list = languageList |> List.rev

let excitingList (languageList: string list) : bool =
    match languageList with
    | [ _; "F#" ]
    | [ _; "F#"; _ ] -> true
    | head :: _tail -> head = "F#"
    | _ -> false
