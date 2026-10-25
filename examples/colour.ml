type t = Red | Green | Blue

let all = [ Red; Green; Blue ]
let to_string = function Red -> "red" | Green -> "green" | Blue -> "blue"
let colour = Rowtype.enum to_string all
