open Decorated_string
open Mods
open Utils

module type Theme = sig
    val left  : unit -> string
    val right : unit -> string
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
        let status_style x = match status with
            | Fail -> (x |> foreground Red)
            | Success -> (x |> foreground Green)
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
                ^ git_end esc
        )
        ^ (match nix with
            | NotInNixShell -> ""
            | _ ->
                nix_start esc
                ^ (text (string_of_nix nix) |> foreground Magenta |> render ~esc)
                ^ nix_end esc
            )
        ^ " "

    let right () = ""
end

module NixIntro : Theme = struct
    let start = text "⋊> "
        |> foreground Blue
        |> render ~esc:(get_esc ())
    let paren = Hex 0x688eb3
    let grey = Hex 0x676767

    let left () =
        let esc = get_esc () in
        let git = git_info () in
        let nix = detect_nix_shell () in
        start
        ^ (text (cwd_basename ()) |> foreground (Hex 0xEEFEFF) |> bold |> render ~esc)
        ^ (match git with
            | NotInGitRepo -> ""
            | _ ->
                " "
                ^ (text "git:(" |> foreground paren |> render ~esc)
                ^ (text (string_of_git git) |> foreground BrightRed |> render ~esc)
                ^ (text ")" |> foreground paren |> render ~esc))
        ^ (match nix with
            | NotInNixShell -> ""
            | _ ->
                " "
                ^ (text "nix:(" |> foreground paren |> render ~esc)
                ^ (text (string_of_nix nix) |> foreground Cyan |> render ~esc)
                ^ (text ")" |> foreground paren |> render ~esc))
        ^ " "
    let right () = text (time ()) |> foreground grey |> render
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

module Default : Theme = struct
    let left () = user_name () ^ "@" ^ host_name () ^ " " ^ cwd_basename () ^ " %# "
    let right () = ""
end

let match_to_theme s =
    match String.lowercase_ascii s with
    | "vwm" -> Some (Vwm.left, Vwm.right)
    | "nix-intro" -> Some (NixIntro.left, NixIntro.right)
    | "robby" -> Some (Robby.left, Robby.right)
    | "tomita" -> Some (Tomita.left, Tomita.right)
    | "default" -> Some (Default.left, Default.right)
    | _ -> None
