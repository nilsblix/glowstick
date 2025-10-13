open Decorated_string
open Utils
open Mods

let () =
    let cwd = cwd_abbreviated () in
    let git = git_info () in
    let nix = detect_nix_shell () in

    let esc = get_esc () in

    let grey = Hex "0x545454" in

    let left = "" in
    let left =
        decorate (left ^ cwd)
        |> foreground (Hex "0xC9D3E1")
        |> append_to_ansi "" esc
    in

    let left =
        match (git, nix) with
        | NotInGitRepo, NotInNixShell -> left
        | _ -> left ^ (decorate " : " |> foreground grey |> append_to_ansi "" esc)
    in

    let left =
        match git with
        | NotInGitRepo -> left
        | _ ->
            decorate (string_of_git git)
            |> foreground Yellow
            |> append_to_ansi left esc
    in
    let left =
        match nix with
        | NotInNixShell -> left
        | _ ->
            decorate (" (" ^ string_of_nix nix ^ ")")
            |> foreground BrightBlue
            |> append_to_ansi left esc
    in
    let left = decorate " ⋊> " |> foreground (Hex "0xDDDDFF") |> append_to_ansi left esc in

    (* let right = decorate (time ()) |> foreground grey |> append_to_ansi "" esc in *)
    let right = "Testing. Testing!" in

    print_prompt left right
