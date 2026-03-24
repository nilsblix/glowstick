open Decorated_string
open Mods
open Utils

module type Theme = sig
  val left : unit -> string
  val right : unit -> string
end

let shorten_nix nix = match nix with
  | NotInNixShell -> ""
  | Pure -> "pure"
  | Impure -> "imp"
  | Unknown -> "unkw"

module Groovy : Theme = struct
  let y = Hex 0xd79921

  let left () =
    let esc = get_esc () in
    let status_style x =
      match last_status () with
      | Fail -> x |> foreground Red
      | Success -> x |> foreground Green
    in
    (text "➜  " |> status_style |> render ~esc)
    ^ (text (Mods.cwd_abbreviated ())
      |> foreground (Hex 0xc0c5cf) |> bold |> render ~esc)
    ^ (let git = git_info () in
       match git with
       | NotInGitRepo -> ""
       | _ ->
           " "
           ^ (text (string_of_git git) |> foreground y |> bold |> render ~esc))
    ^ (let nix = detect_nix_shell () in
       match nix with
       | NotInNixShell -> ""
       | _ ->
           " "
           ^ (text (shorten_nix nix) |> foreground Red |> bold |> render ~esc))
    ^ (text " * " |> foreground Green |> render ~esc)

  let right () = ""
end

module EnhancedDef : Theme = struct
  let left () =
    let esc = get_esc () in
    let status_style x =
      match last_status () with
      | Fail -> x |> foreground Red
      | Success -> x |> foreground Green
    in
    (text "➜  " |> status_style |> render ~esc)
    ^ (text (Mods.cwd_basename ())
      |> foreground (Hex 0xc0c5cf) |> bold |> render ~esc)
    ^ (let git = git_info () in
       match git with
       | NotInGitRepo -> ""
       | _ ->
           " "
           ^ (text (string_of_git git) |> foreground Cyan |> bold |> render ~esc))
    ^ (let nix = detect_nix_shell () in
       match nix with
       | NotInNixShell -> ""
       | _ ->
           " "
           ^ (text (string_of_nix nix) |> foreground Red |> bold |> render ~esc))
    ^ " "

  let right () = ""
end

module Vwm : Theme = struct
  let cwd x = x |> foreground (Hex 0xd5e0f7) |> bold
  let extras x = x |> foreground Blue |> bold
  let git_start esc = text " git:(" |> extras |> render ~esc
  let git_end esc = text ")" |> extras |> render ~esc
  let nix_start esc = text " nix:(" |> extras |> render ~esc
  let nix_end esc = text ")" |> extras |> render ~esc

  let left () =
    let esc = get_esc () in
    let status = last_status () in
    let status_style x =
      match status with
      | Fail -> x |> foreground Red
      | Success -> x |> foreground Green
    in
    let git = git_info () in
    let nix = detect_nix_shell () in
    (text "➜  " |> status_style |> render ~esc)
    ^ (text (cwd_basename ()) |> cwd |> render ~esc)
    ^ (match git with
      | NotInGitRepo -> ""
      | _ ->
          git_start esc
          ^ (text (string_of_git git) |> foreground Red |> render ~esc)
          ^ git_end esc)
    ^ (match nix with
      | NotInNixShell -> ""
      | _ ->
          nix_start esc
          ^ (text (string_of_nix nix) |> foreground Magenta |> render ~esc)
          ^ nix_end esc)
    ^ " "

  let right () = ""
end

module Tomita : Theme = struct
  let green = Hex 0x008B67
  let white = Hex 0xFFFFFF
  let yellow = Hex 0xE2D351
  let grey = Hex 0x676767

  let left () =
    let cwd = cwd_abbreviated () in
    let git = git_info () in
    let esc = get_esc () in
    (text cwd |> foreground green |> render ~esc)
    ^ (match git with
      | NotInGitRepo -> ""
      | _ ->
          text (" (" ^ string_of_git git ^ ")")
          |> foreground white |> render ~esc)
    ^ (text " ⋊> " |> foreground yellow |> render ~esc)

  let right () =
    let nix = detect_nix_shell () in
    let time = time () in
    let esc = get_esc () in
    (match nix with
    | NotInNixShell -> ""
    | _ ->
        text ("(nix: " ^ string_of_nix nix ^ ") ")
        |> foreground grey |> render ~esc)
    ^ (text time |> foreground grey |> render ~esc)
end

module Default : Theme = struct
  let left () =
    user_name () ^ "@" ^ host_name () ^ ":" ^ cwd_basename () ^ " $ "

  let right () = ""
end

let match_to_theme (s : string) =
  match String.lowercase_ascii s with
  | "groovy" -> Some (Groovy.left, Groovy.right)
  | "enhanced-def" -> Some (EnhancedDef.left, EnhancedDef.right)
  | "vwm" -> Some (Vwm.left, Vwm.right)
  | "tomita" -> Some (Tomita.left, Tomita.right)
  | "default" -> Some (Default.left, Default.right)
  | _ -> None
