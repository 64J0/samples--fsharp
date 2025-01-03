let private lyricsChanges =
    [ ("first", "a Partridge in a Pear Tree.")
      ("second", "two Turtle Doves, and ")
      ("third", "three French Hens, ")
      ("fourth", "four Calling Birds, ")
      ("fifth", "five Gold Rings, ")
      ("sixth", "six Geese-a-Laying, ")
      ("seventh", "seven Swans-a-Swimming, ")
      ("eighth", "eight Maids-a-Milking, ")
      ("ninth", "nine Ladies Dancing, ")
      ("tenth", "ten Lords-a-Leaping, ")
      ("eleventh", "eleven Pipers Piping, ")
      ("twelfth", "twelve Drummers Drumming, ") ]

let private combinedGifts =
    lyricsChanges
    |> List.map snd
    |> List.scan (fun acc el -> el + acc) ""
    |> List.tail

let recite (start: int) (stop: int) : string list =
    let dayNumber = (lyricsChanges |> List.map fst).[(start - 1) .. (stop - 1)]
    let gifts = combinedGifts.[(start - 1) .. (stop - 1)]

    List.zip dayNumber gifts
    |> List.map (fun (d, g) -> sprintf "On the %s day of Christmas my true love gave to me: %s" d g)
