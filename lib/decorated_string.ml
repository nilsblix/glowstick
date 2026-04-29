type color =
  | Red
  | Green
  | Yellow
  | Blue
  | Magenta
  | Cyan
  | White
  | Black
  | BrightRed
  | BrightGreen
  | BrightYellow
  | BrightBlue
  | BrightMagenta
  | BrightCyan
  | BrightWhite
  | BrightBlack
  | Rgb of (int * int * int)
  | Hex of int

let rgb_of_hex (n : int) =
  let r = (n lsr 16) land 0xFF in
  let g = (n lsr 8) land 0xFF in
  let b = n land 0xFF in
  (r, g, b)

let rec color_to_ansi (color : color) =
  match color with
  | Black -> "30"
  | Red -> "31"
  | Green -> "32"
  | Yellow -> "33"
  | Blue -> "34"
  | Magenta -> "35"
  | Cyan -> "36"
  | White -> "37"
  | BrightBlack -> "90"
  | BrightRed -> "91"
  | BrightGreen -> "92"
  | BrightYellow -> "93"
  | BrightBlue -> "94"
  | BrightMagenta -> "95"
  | BrightCyan -> "96"
  | BrightWhite -> "97"
  | Rgb (r, g, b) ->
      let i = string_of_int in
      "38;2;" ^ i r ^ ";" ^ i g ^ ";" ^ i b
  | Hex n -> color_to_ansi (Rgb (rgb_of_hex n))

let rec color_to_ansi_bg (color : color) =
  match color with
  | Black -> "40"
  | Red -> "41"
  | Green -> "42"
  | Yellow -> "43"
  | Blue -> "44"
  | Magenta -> "45"
  | Cyan -> "46"
  | White -> "47"
  | BrightBlack -> "100"
  | BrightRed -> "101"
  | BrightGreen -> "102"
  | BrightYellow -> "103"
  | BrightBlue -> "104"
  | BrightMagenta -> "105"
  | BrightCyan -> "106"
  | BrightWhite -> "107"
  | Rgb (r, g, b) ->
      let i = string_of_int in
      "48;2;" ^ i r ^ ";" ^ i g ^ ";" ^ i b
  | Hex n -> color_to_ansi_bg (Rgb (rgb_of_hex n))

type t =
  | Bold of t
  | Foreground of (t * color)
  | Background of (t * color)
  | Underlined of t
  | Italic of t
  | Padded of (t * string * string)
  | Text of string

let text s = Text s
let bold d = Bold d
let foreground col d = Foreground (d, col)
let background col d = Background (d, col)
let underlined d = Underlined d
let italic d = Italic d
let padded left right d = Padded (d, left, right)

let render (esc : (string -> string) option) (dec : t) =
  let escape_fun = match esc with
    | Some f -> f
    | None -> Option.value (Utils.get_esc ()) ~default:Utils.default_esc
  in
  let rec aux acc d =
    match d with
    | Bold inner ->
        let acc = acc ^ escape_fun "\x1b[1m" in
        let acc = aux acc inner in
        acc ^ escape_fun "\x1b[22m"
    | Foreground (inner, color) ->
        let acc = acc ^ escape_fun ("\x1b[" ^ color_to_ansi color ^ "m") in
        let acc = aux acc inner in
        acc ^ escape_fun "\x1b[39m"
    | Background (inner, color) ->
        let acc = acc ^ escape_fun ("\x1b[" ^ color_to_ansi_bg color ^ "m") in
        let acc = aux acc inner in
        acc ^ escape_fun "\x1b[49m"
    | Underlined inner ->
        let acc = acc ^ escape_fun "\x1b[4m" in
        let acc = aux acc inner in
        acc ^ escape_fun "\x1b[24m"
    | Italic inner ->
        let acc = acc ^ escape_fun "\x1b[3m" in
        let acc = aux acc inner in
        acc ^ escape_fun "\x1b[23m"
    | Padded (inner, left, right) ->
        let acc = aux acc inner in
        let reset = "\x1b[0m" in
        let left = reset ^ left in
        let right = reset ^ right in
        let acc = left ^ acc ^ right in
        acc
    | Text s -> acc ^ s
  in
  aux "" dec
