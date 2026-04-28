open Decorated_string
open Mods
open Utils

module type Theme = sig
  val left : unit -> string
  val right : unit -> string
end

module Anyhow : Theme = struct
  let left () =
    let esc = get_esc () in
    let start = text "➜  "
      |> foreground (Hex 0x76A8E0)
      |> render ~esc
    in
    let dir = cwd_basename () in
    let git = match git_info () with
    | NotInGitRepo -> ""
    | Branch b ->
      " ("
      ^ (text b
        (* |> foreground (Hex 0xD78A84) *)
        |> bold
        |> render ~esc)
      ^ ")"
    in
    let nix = match detect_nix_shell () with
    | NotInNixShell -> ""
    | n ->
        " (\u{f1105}"
        ^ (text (string_of_nix n)
          (* |> foreground (Hex 0xF9ED7D) *)
          |> bold
          |> render ~esc)
        ^ ")"
    in
    let status = match last_status () with
    | Success -> ""
    | Fail code ->
      " ("
      ^ (text (string_of_int code)
        |> foreground Red
        |> render ~esc)
      ^ ")"
    in
    let marker = text " $ "
      |> foreground (Hex 0x80B768)
      |> render ~esc
    in
    start ^ dir ^ git ^ nix ^ status ^ marker

  let right () = ""
end

let shorten_nix nix = match nix with
  | NotInNixShell -> ""
  | Pure -> "@P"
  | Impure -> "@I"
  | Unknown -> "@U"

module Fish : Theme = struct
  let left () =
    let esc = get_esc () in
    let status_style x =
      match last_status () with
      | Fail _ -> x |> foreground Red
      | Success -> x |> foreground Green
    in
    (text "⋊>  " |> status_style |> render ~esc)
    ^ (text (cwd_abbreviated ())
      |> foreground White
      |> bold
      |> render ~esc)
    ^ (let git = git_info () in
       match git with
       | NotInGitRepo -> ""
       | _ -> text (" " ^ string_of_git git)
              |> foreground (Hex 0xC8F741)
              |> render ~esc)
    ^ (let nix = detect_nix_shell () in
       match nix with
       | NotInNixShell -> ""
       | _ -> text (" " ^ shorten_nix nix)
              |> foreground (Hex 0xF48FB1)
              |> bold
              |> render ~esc)
    ^ (text " > " |> foreground Magenta |> bold |> render ~esc)

  let right () = ""
end

module Default : Theme = struct
  let left () =
    user_name () ^ "@" ^ host_name () ^ " " ^ cwd_basename () ^ " $ "

  let right () = ""
end

let match_to_theme (s : string) =
  match String.lowercase_ascii s with
  | "anyhow" -> Some (Anyhow.left, Fish.right)
  | "fish" -> Some (Fish.left, Fish.right)
  | "default" -> Some (Default.left, Default.right)
  | _ -> None
