open Decorated_string
open Mods
open Utils

module type Theme = sig
  val left : unit -> string
  val right : unit -> string
end

module Default : Theme = struct
  let left () =
    user_name () ^ "@" ^ host_name () ^ " " ^ cwd_basename () ^ " $ "

  let right () = ""
end

let default_nix_segment () =
  match detect_nix_shell () with
  | NotInNixShell -> ""
  | nix ->
      text (" (nix:" ^ string_of_nix nix ^ ")")
      |> foreground (Hex 0xF48FB1) |> bold |> render (get_esc ())

module DefaultNix : Theme = struct
  let left () =
    user_name () ^ "@" ^ host_name () ^ " " ^ cwd_basename ()
    ^ default_nix_segment () ^ " $ "

  let right () = ""
end

let fish_prompt_hostname () =
  let host = host_name () in
  match String.index_opt host '.' with
  | Some idx -> String.sub host 0 idx
  | None -> host

let fish_vcs_prompt () =
  match git_info () with
  | NotInGitRepo -> ""
  | Branch b -> " (" ^ b ^ ")"

let fish_prompt_status esc =
  match last_status () with
  | Success -> ""
  | Fail code ->
      " "
      ^ (text ("[" ^ string_of_int code ^ "]") |> foreground Red |> render esc)

module DefaultFish : Theme = struct
  let left () =
    let esc = get_esc () in
    let cwd_color, suffix =
      if Unix.geteuid () = 0 then (Red, "#") else (Green, ">")
    in
    let host =
      let h = text (fish_prompt_hostname ()) in
      match Sys.getenv_opt "SSH_TTY" with
      | Some _ -> h |> foreground Yellow |> render esc
      | None -> render esc h
    in
    (text (user_name ()) |> foreground BrightGreen |> render esc)
    ^ "@" ^ host ^ " "
    ^ (text (cwd_abbreviated ()) |> foreground cwd_color |> render esc)
    ^ fish_vcs_prompt () ^ fish_prompt_status esc ^ suffix ^ " "

  let right () = ""
end

module Anyhow : Theme = struct
  let left () =
    let esc = get_esc () in
    let start =
      let c = match last_status () with Success -> Green | Fail _ -> Red in
      text "➜  " |> foreground c |> render esc
    in
    let dir = cwd_basename () in
    let git =
      match git_info () with
      | NotInGitRepo -> ""
      | Branch b ->
          text b |> padded " " ""
          |> foreground White (* (Hex 0xD4E057) *)
          |> bold |> render esc
    in
    let nix =
      match detect_nix_shell () with
      | NotInNixShell -> ""
      | n ->
          let diamond = function
            | x -> text x |> foreground (Hex 0x316ce4) |> render esc
          in
          text (string_of_nix n)
          |> background (Hex 0x316ce4) |> foreground White |> bold
          |> padded (diamond " \u{e0b6}") (diamond "\u{e0b4}")
          |> render esc
    in
    let marker = text " $ " |> foreground (Hex 0x80B768) |> render esc in
    start ^ dir ^ git ^ nix ^ marker

  let right () = ""
end

let shorten_nix nix =
  match nix with
  | NotInNixShell -> ""
  | Pure -> "@P"
  | Impure -> "@I"
  | Unknown -> "@U"

module Modern : Theme = struct
  let left () =
    let esc = get_esc () in
    let status_style x =
      match last_status () with
      | Fail _ -> x |> foreground Red
      | Success -> x |> foreground Green
    in
    (* ⋊> *)
    (text "➜  " |> status_style |> render esc)
    ^ (text (cwd_abbreviated ()) |> foreground White |> bold |> render esc)
    ^ (let git = git_info () in
       match git with
       | NotInGitRepo -> ""
       | _ ->
           text (" " ^ string_of_git git)
           |> foreground (Hex 0xC8F741) |> render esc)
    ^ (let nix = detect_nix_shell () in
       match nix with
       | NotInNixShell -> ""
       | _ ->
           text (" " ^ shorten_nix nix)
           |> foreground (Hex 0xF48FB1) |> bold |> render esc)
    ^ (text " > " |> foreground Magenta |> bold |> render esc)

  let right () = ""
end

let match_to_theme (s : string) =
  match String.lowercase_ascii s with
  | "default" -> Some (Default.left, Default.right)
  | "default-nix" -> Some (DefaultNix.left, DefaultNix.right)
  | "default-fish" | "fish" -> Some (DefaultFish.left, DefaultFish.right)
  | "anyhow" -> Some (Anyhow.left, Anyhow.right)
  | "modern" -> Some (Modern.left, Modern.right)
  | _ -> None
