open Decorated_string
open Mods
open Utils

module type Theme = sig
    val left  : unit -> string
    val right : unit -> string
end

module Robby : Theme = struct
    let start = text "⋊> "
        |> foreground Blue
        |> render ~esc:(get_esc ())
    let purple = Hex 0x6f5ad6
    let grey = Hex 0x676767

    let left () =
        let esc = get_esc () in
        let git = git_info () in
        let nix = detect_nix_shell () in
        start
        ^ (text (cwd_basename ()) |> foreground (Hex 0xCDDDFF) |> bold |> render ~esc)
        ^ (match git with
            | NotInGitRepo -> ""
            | _ ->
                " "
                ^ (text "git:(" |> foreground purple |> bold |> render ~esc)
                ^ (text (string_of_git git) |> foreground BrightRed |> bold |> render ~esc)
                ^ (text ")" |> foreground purple |> bold |> render ~esc))
        ^ (match nix with
            | NotInNixShell -> ""
            | _ ->
                " "
                ^ (text "nix:(" |> foreground purple |> bold |> render ~esc)
                ^ (text (string_of_nix nix) |> foreground Yellow |> bold |> render ~esc)
                ^ (text ")" |> foreground purple |> bold |> render ~esc))
        ^ " "
    let right () = text (time ()) |> foreground grey |> render

end

module Current : Theme = struct
    let start = text " ⋊>"
        |> foreground (Hex 0x144ac9)
        |> render ~esc:(get_esc ())
    let blue_fg = Hex 0x379eed
    let grey_fg = Hex 0x676767

    let left () =
        let esc = get_esc () in
        let git = git_info () in
        start ^ (text (" " ^ cwd_basename ())
            |> foreground blue_fg
            |> render ~esc)
        ^ (match git with
            | NotInGitRepo -> ""
            | _ -> text (" (" ^ string_of_git git ^ ")")
                |> foreground blue_fg |> render ~esc)
        ^ " "

    let right () =
        let nix = detect_nix_shell () in
        let time = time () in
        let esc = get_esc () in
        (match nix with
            | NotInNixShell -> ""
            | _ -> text ("(nix: " ^ string_of_nix nix ^ ") ")
                |> foreground grey_fg
                |> render ~esc)
        ^ (text time |> foreground grey_fg |> render ~esc)
end

module Macos : Theme = struct
    let fg = Hex 0xE0E0E0
    let grey = Hex 0x676767

    let left () =
        let esc = get_esc () in
        let git = git_info () in
        (text (user_name () ^ "@" ^ host_name () ^ " " ^ cwd_basename ()
            ^ (match git with | NotInGitRepo -> "" | _ ->
                " (" ^ string_of_git git ^ ")"))
            |> foreground fg
            |> render ~esc)
        ^ (text " ⋊> " |> foreground Yellow |> render ~esc)

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

module Sushi : Theme = struct
    let red = Red
    let yellow = Yellow
    let grey = Hex 0x676767

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

let match_to_theme s =
    match String.lowercase_ascii s with
    | "robby" -> Some (Robby.left, Robby.right)
    | "current" -> Some (Current.left, Current.right)
    | "macos" -> Some (Macos.left, Macos.right)
    | "sushi" -> Some (Sushi.left, Sushi.right)
    | "tomita" -> Some (Tomita.left, Tomita.right)
    | _ -> None
