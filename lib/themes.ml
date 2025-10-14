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
        (decorate "(" |> foreground col |> append_to_ansi "" esc)
        ^ s
        ^ (decorate ")" |> foreground col |> append_to_ansi "" esc)

    let left () =
        let cwd = cwd_abbreviated () in
        let esc = get_esc () in
        wrap (decorate cwd |> foreground yellow |> append_to_ansi "" esc) red esc
        ^ (decorate " λ " |> foreground red |> append_to_ansi "" esc)

    let right () =
        let git = git_info () in
        let nix = detect_nix_shell () in
        let esc = get_esc () in
        (match git with
            | NotInGitRepo -> ""
            | _ -> wrap
            (decorate ("git: " ^ string_of_git git) |> foreground grey |> append_to_ansi "" esc)
            yellow esc)
        ^ (match git, nix with
            | NotInGitRepo, _ | _, NotInNixShell -> ""
            | _ -> " ")
        ^ (match nix with
            | NotInNixShell -> ""
            | _ -> wrap
            (decorate ("nix: " ^ string_of_nix nix) |> foreground grey |> append_to_ansi "" esc)
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
        (decorate cwd |> foreground green |> append_to_ansi "" esc)
        ^ (match git with
            | NotInGitRepo -> ""
            | _ -> decorate (" (" ^ string_of_git git^ ")")
                    |> foreground white
                    |> append_to_ansi "" esc)
        ^ (decorate  " ⋊> " |> foreground yellow |> append_to_ansi "" esc)

    let right () =
        let nix = detect_nix_shell () in
        let time = time () in
        let esc = get_esc () in
        (match nix with
        | NotInNixShell -> ""
        | _ -> decorate ("(nix: " ^ string_of_nix nix ^ ") ")
                |> foreground grey
                |> append_to_ansi "" esc)
        ^ (decorate time |> foreground grey |> append_to_ansi "" esc)
end
