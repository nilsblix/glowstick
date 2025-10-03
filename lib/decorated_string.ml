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
    | Hex of string

let rgb_of_hex n =
    let r = (n lsr 16) land 0xFF in
    let g = (n lsr 8) land 0xFF in
    let b = n land 0xFF in
    (r, g, b)

let rec color_to_ansi color = match color with
    | Black         -> "30"
    | Red           -> "31"
    | Green         -> "32"
    | Yellow        -> "33"
    | Blue          -> "34"
    | Magenta       -> "35"
    | Cyan          -> "36"
    | White         -> "37"
    | BrightBlack   -> "90"
    | BrightRed     -> "91"
    | BrightGreen   -> "92"
    | BrightYellow  -> "93"
    | BrightBlue    -> "94"
    | BrightMagenta -> "95"
    | BrightCyan    -> "96"
    | BrightWhite   -> "97"
    | Rgb (r, g, b) -> let i = string_of_int in
        "38;2;" ^ i r ^ ";" ^ i g ^ ";" ^ i b
    | Hex s -> color_to_ansi (Rgb (rgb_of_hex (int_of_string s)))

let rec color_to_ansi_bg color = match color with
    | Black         -> "40"
    | Red           -> "41"
    | Green         -> "42"
    | Yellow        -> "43"
    | Blue          -> "44"
    | Magenta       -> "45"
    | Cyan          -> "46"
    | White         -> "47"
    | BrightBlack   -> "100"
    | BrightRed     -> "101"
    | BrightGreen   -> "102"
    | BrightYellow  -> "103"
    | BrightBlue    -> "104"
    | BrightMagenta -> "105"
    | BrightCyan    -> "106"
    | BrightWhite   -> "107"
    | Rgb (r, g, b) -> let i = string_of_int in
        "48;2;" ^ i r ^ ";" ^ i g ^ ";" ^ i b
    | Hex s -> color_to_ansi_bg (Rgb (rgb_of_hex (int_of_string s)))

type decorated_string =
    | Bold of decorated_string
    | Foreground of (decorated_string * color)
    | Background of (decorated_string * color)
    | Underlined of decorated_string
    | Italic of decorated_string
    | Default of string

let bold dec = Bold dec
let foreground col dec = Foreground (dec, col)
let background col dec = Background (dec, col)
let underlined dec = Underlined dec
let italic dec = Italic dec
let decorate s = Default s

let rec append_to_ansi s escape_fun dec = match dec with
    | Bold inner ->
        let s = s ^ escape_fun "\x1b[1m" in
        let s = append_to_ansi s escape_fun inner in
        s ^ escape_fun "\x1b[22m"
    | Foreground (inner, color) ->
        let s = s ^ escape_fun ("\x1b[" ^ color_to_ansi color ^ "m") in
        let s = append_to_ansi s escape_fun inner in
        s ^ escape_fun "\x1b[39m"
    | Background (inner, color) ->
        let s = s ^ escape_fun ("\x1b[" ^ color_to_ansi_bg color ^ "m") in
        let s = append_to_ansi s escape_fun inner in
        s ^ escape_fun "\x1b[49m"
    | Underlined inner ->
        let s = s ^ escape_fun "\x1b[4m" in
        let s = append_to_ansi s escape_fun inner in
        s ^ escape_fun "\x1b[24m"
    | Italic inner ->
        let s = s ^ escape_fun "\x1b[3m" in
        let s = append_to_ansi s escape_fun inner in
        s ^ escape_fun "\x1b[23m"
    | Default text -> s ^ text
