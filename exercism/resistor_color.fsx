let colors: string list =
    [ "black"
      "brown"
      "red"
      "orange"
      "yellow"
      "green"
      "blue"
      "violet"
      "grey"
      "white" ]

let colorCode (color: string) : int =
    List.findIndex (fun x -> x = color) colors
