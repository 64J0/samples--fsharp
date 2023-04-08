module RotationalCipher

let alphabetSize = 26

let rotateChar (shiftKey: int) (c: char) =
    if not (System.Char.IsLetter c) then c
    else
        let last = if System.Char.IsLower c then int 'z' else int 'Z'
        let rotatedChar = int c + shiftKey
        if rotatedChar > last then char (rotatedChar - alphabetSize) else char (rotatedChar)
        
let rotate (shiftKey: int) (text: string) =
    let adjustedShiftKey = shiftKey % alphabetSize

    text
    |> String.map (rotateChar adjustedShiftKey)

rotate 5 "OMG"
