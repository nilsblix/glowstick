open Decorated_string
open Mods
open Utils

module type Theme = sig
    val left  : unit -> string
    val right : unit -> string
end

module Sushi : Theme = struct
    let red = Red
    let yellow = Yellow
    let grey = Hex "0x676767"

    let wrap s col esc =
        (text "(" |> foreground col |> render ~esc)
        ^ s
        ^ (text ")" |> foreground col |> render ~esc)

    let left () =
        let cwd = cwd_abbreviated () in
        let esc = get_esc () in
        wrap (text cwd |> foreground yellow |> render ~esc) red esc
        ^ (text " λ " |> foreground red |> render ~esc)

    let right () =
        let git = git_info () in
        let nix = detect_nix_shell () in
        let esc = get_esc () in
        (match git with
            | NotInGitRepo -> ""
            | _ -> wrap
            (text ("git: " ^ string_of_git git) |> foreground grey |> render ~esc)
            yellow esc)
        ^ (match git, nix with
            | NotInGitRepo, _ | _, NotInNixShell -> ""
            | _ -> " ")
        ^ (match nix with
            | NotInNixShell -> ""
            | _ -> wrap
            (text ("nix: " ^ string_of_nix nix) |> foreground grey |> render ~esc)
            yellow esc)
end

module Tomita : Theme = struct
    let green = Hex "0x008B67"
    let white = Hex "0xFFFFFF"
    let yellow = Hex "0xE2D351"
    let grey = Hex "0x676767"

    let left () =
        let cwd = cwd_abbreviated () in
        let git = git_info () in
        let esc = get_esc () in
        (text cwd |> foreground green |> render ~esc)
        ^ (match git with
            | NotInGitRepo -> ""
            | _ -> text (" (" ^ string_of_git git^ ")")
                    |> foreground white
                    |> render ~esc)
        ^ (text  " ⋊> " |> foreground yellow |> render ~esc)

    let right () =
        let nix = detect_nix_shell () in
        let time = time () in
        let esc = get_esc () in
        (match nix with
        | NotInNixShell -> ""
        | _ -> text ("(nix: " ^ string_of_nix nix ^ ") ")
                |> foreground grey
                |> render ~esc)
        ^ (text time |> foreground grey |> render ~esc)
end

module Agnoster : Theme = struct
    let blue = Hex "0x268BD2"
    let cyan = Hex "0x2AA198"
    let yellow = Hex "0xB58900"
    let white = Hex "0xFFFFFF"
    let grey = Hex "0x666666"

    let left () =
        let esc = get_esc () in
        let userhost = Mods.user_name () ^ "@" ^ Mods.host_name () in
        let cwd = cwd_abbreviated () in
        let git = git_info () in
        (text userhost |> bold |> foreground white |> background blue |> render ~esc)
        ^ (text " " |> background blue |> render ~esc)
        ^ (text cwd |> foreground cyan |> render ~esc)
        ^ (match git with
            | NotInGitRepo -> ""
            | _ ->
                (text ("  " ^ string_of_git git))
                |> foreground yellow |> render ~esc)
        ^ (text " ❯ " |> foreground blue |> render ~esc)

    let right () =
        let esc = get_esc () in
        let nix = detect_nix_shell () in
        let t = time () in
        (match nix with
            | NotInNixShell -> ""
            | _ -> text ("nix:" ^ string_of_nix nix ^ " ") |> foreground grey |> render ~esc)
        ^ (text t |> foreground grey |> render ~esc)
end

module BobTheFish : Theme = struct
    let blue = Hex "0x4F97D7"
    let magenta = Hex "0xC678DD"
    let green = Hex "0x98C379"
    let grey = Hex "0x676767"

    let left () =
        let esc = get_esc () in
        let cwd = cwd_abbreviated () in
        let git = git_info () in
        (text cwd |> foreground blue |> render ~esc)
        ^ (match git with
            | NotInGitRepo -> ""
            | _ -> text (" · " ^ string_of_git git) |> foreground magenta |> render ~esc)
        ^ (text " ❯ " |> foreground green |> render ~esc)

    let right () =
        let esc = get_esc () in
        let nix = detect_nix_shell () in
        let t = time () in
        (match nix with
            | NotInNixShell -> ""
            | _ -> text ("(nix " ^ string_of_nix nix ^ ") ") |> foreground grey |> render ~esc)
        ^ (text t |> foreground grey |> render ~esc)
end

module Lambda : Theme = struct
    let purple = Hex "0xA277FF"
    let gold = Hex "0xFFCA3A"
    let grey = Hex "0x676767"

    let left () =
        let esc = get_esc () in
        let cwd = cwd_abbreviated () in
        let git = git_info () in
        (text "λ " |> foreground purple |> render ~esc)
        ^ (text cwd |> foreground purple |> render ~esc)
        ^ (match git with
            | NotInGitRepo -> ""
            | _ -> text (" (" ^ string_of_git git ^ ")") |> foreground gold |> render ~esc)
        ^ (text " » " |> foreground purple |> render ~esc)

    let right () =
        let esc = get_esc () in
        let t = time () in
        text t |> foreground grey |> render ~esc
end

module Minimal : Theme = struct
    let green = Hex "0x00C853"
    let grey = Hex "0x676767"

    let left () =
        let esc = get_esc () in
        let cwd = cwd_basename () in
        (text cwd |> foreground green |> render ~esc)
        ^ (text " > " |> foreground green |> render ~esc)

    let right () =
        let esc = get_esc () in
        let git = git_info () in
        match git with
        | NotInGitRepo -> ""
        | _ -> text (string_of_git git) |> foreground grey |> render ~esc
end

let match_to_theme s =
  match String.lowercase_ascii s with
  | "sushi" -> Some (Sushi.left, Sushi.right)
  | "tomita" -> Some (Tomita.left, Tomita.right)
  | "agnoster" -> Some (Agnoster.left, Agnoster.right)
  | "bobthefish" | "bob_the_fish" -> Some (BobTheFish.left, BobTheFish.right)
  | "lambda" -> Some (Lambda.left, Lambda.right)
  | "minimal" -> Some (Minimal.left, Minimal.right)
  | _ -> None
