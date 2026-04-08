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

module Fish : Theme = struct
  let left () =
    let esc = get_esc () in
    let status_style x =
      match last_status () with
      | Fail -> x |> foreground Red
      | Success -> x |> foreground Green
    in
    (text "⋊>  " |> status_style |> render ~esc)
    ^ (text (cwd_basename ()) |> bold |> render ~esc)
    ^ (let git = git_info () in
       match git with
       | NotInGitRepo -> ""
       | _ -> " " ^ string_of_git git)
    ^ (let nix = detect_nix_shell () in
       match nix with
       | NotInNixShell -> ""
       | _ -> " " ^ shorten_nix nix)
    ^ (text " > " |> foreground Magenta |> bold |> render ~esc)

  let right () = ""
end

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

module Hyperion : Theme = struct
  let left () =
    let esc = get_esc () in
    let status_style x =
      match last_status () with
      | Fail -> x |> foreground Red
      | Success -> x |> foreground (Hex 0x89C1FE)
    in
    (text "➜ " |> status_style |> render ~esc)
    ^ user_name () ^ "@" ^ host_name () ^ " " ^ cwd_basename ()
    ^ (let nix = detect_nix_shell () in
       match nix with
       | NotInNixShell -> ""
       | _ ->
           " "
           ^ (text (shorten_nix nix) |> foreground Red |> bold |> render ~esc))
    ^ (text "> " |> foreground (Hex 0x4287f5) |> render ~esc)

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

module Default : Theme = struct
  let left () =
    user_name () ^ "@" ^ host_name () ^ " " ^ cwd_basename () ^ " $ "

  let right () = ""
end

let match_to_theme (s : string) =
  match String.lowercase_ascii s with
  | "fish" -> Some (Fish.left, Fish.right)
  | "groovy" -> Some (Groovy.left, Groovy.right)
  | "hyperion" -> Some (Hyperion.left, Hyperion.right)
  | "vwm" -> Some (Vwm.left, Vwm.right)
  | "default" -> Some (Default.left, Default.right)
  | _ -> None
