open Decorated
open Decorated_string

let remove_prefix prefix s =
    let len_prefix = String.length prefix in
    if String.starts_with ~prefix s then
        Some (String.sub s len_prefix (String.length s - len_prefix))
    else
        None

let contains_substring needle (haystack: string) =
    let n = String.length needle in
    let h = String.length haystack in
    if n = 0 then true else
        let rec scan i =
            if i + n > h then false
            else if String.sub haystack i n = needle then true
            else scan (i + 1)
        in
        scan 0

let user_name () =
    let uid = Unix.getuid () in
    let pw = Unix.getpwuid uid in
    pw.Unix.pw_name

let host_name () = Unix.gethostname ()

let cwd_label () =
    let cwd = Sys.getcwd () in
    match Sys.getenv_opt "HOME" with
    | Some home -> (match remove_prefix home cwd with
        | Some path -> "~" ^ path
        | None -> cwd)
    | _ -> cwd

let split_on_slash s =
    let len = String.length s in
    let rec loop i start acc =
        if i >= len then
            let seg = String.sub s start (len - start) in
            List.rev (seg :: acc)
        else if s.[i] = '/' then
            let seg = String.sub s start (i - start) in
            loop (i + 1) (i + 1) (seg :: acc)
        else
            loop (i + 1) start acc
    in
    if len = 0 then [] else loop 0 0 []

let abbreviate_segments segments =
    match List.rev segments with
    | [] -> ""
    | last :: rest_rev ->
        let rest = List.rev rest_rev in
        let abbrev_rest = List.map (fun s -> if s = "" then "" else String.sub s 0 1) rest in
        String.concat "/" (abbrev_rest @ [last])

let cwd_label_abbreviated () =
    let cwd = Sys.getcwd () in
    match Sys.getenv_opt "HOME" with
    | Some home when String.starts_with ~prefix:home cwd ->
        if cwd = home then "~"
        else
            let start_idx =
                let l = String.length home in
                if String.length cwd > l && cwd.[l] = '/' then l + 1 else l
            in
            let rel = String.sub cwd start_idx (String.length cwd - start_idx) in
            let segments = split_on_slash rel in
            "~/" ^ abbreviate_segments segments
    | _ ->
        if String.length cwd = 1 && cwd.[0] = '/' then "/"
        else if String.starts_with ~prefix:"/" cwd then
            let nolead = String.sub cwd 1 (String.length cwd - 1) in
            let segments = split_on_slash nolead in
            "/" ^ abbreviate_segments segments
        else
            abbreviate_segments (split_on_slash cwd)

type nix_shell_type = | NotInNixShell | Pure | Impure | Unknown

let detect_nix_shell () =
    match Sys.getenv_opt "IN_NIX_SHELL" with
    | Some "pure" -> Pure
    | Some "impure" -> Impure
    | Some _ -> Unknown
    | None -> match Sys.getenv_opt "PATH" with
        | Some haystack ->
            if contains_substring "/nix/store" haystack then Unknown
            else NotInNixShell
        | None -> NotInNixShell

type git_repo_type = | NotInGitRepo | Branch of string

let read_file path =
    try
        let ic = open_in_bin path in
        let s = Fun.protect ~finally:(fun () -> close_in_noerr ic) (fun () ->
            really_input_string ic (in_channel_length ic)
        ) in
        Ok s
    with e -> Error (Printexc.to_string e)

let rec find_dotgit_dir cur =
    let dotgit = Filename.concat cur ".git" in
    if Sys.file_exists dotgit then Some (cur, dotgit)
    else
        let parent = Filename.dirname cur in
        if parent = cur then None else find_dotgit_dir parent

let resolve_git_dir work_root dotgit =
    if Sys.is_directory dotgit then Some dotgit
    else
        (* .git is a file with a pointer to real gitdir *)
        match read_file dotgit with
        | Ok content ->
            let content = String.trim content in
            (match remove_prefix "gitdir: " content with
                | Some p ->
                    let p = String.trim p in
                    let resolved = if Filename.is_relative p then Filename.concat work_root p else p in
                    Some resolved
                | None -> None)
        | Error _ -> None

let branch_name_of_head head_content =
    let head = String.trim head_content in
    if String.starts_with ~prefix:"ref: " head then
        let refpath = String.sub head 5 (String.length head - 5) |> String.trim in
        match remove_prefix "refs/heads/" refpath with
        | Some b -> b
        | None -> (match remove_prefix "refs/" refpath with | Some b -> b | None -> refpath)
    else
        let n = min 7 (String.length head) in
        "detached@" ^ String.sub head 0 n

let git_info () =
    match find_dotgit_dir (Sys.getcwd ()) with
    | None -> NotInGitRepo
    | Some (work_root, dotgit) ->
        (match resolve_git_dir work_root dotgit with
            | None -> NotInGitRepo
            | Some gitdir ->
                let head_path = Filename.concat gitdir "HEAD" in
                (match read_file head_path with
                    | Ok s -> Branch (branch_name_of_head s)
                    | Error _ -> NotInGitRepo))

let string_of_git git = match git with
    | NotInGitRepo -> ""
    | Branch b -> b

let string_of_nix nix = match nix with
    | NotInNixShell -> ""
    | Pure -> "pure"
    | Impure -> "impure"
    | Unknown -> "unknown"

let get_esc () =
    let default = (fun x -> x) in
    let shell_opt = Sys.getenv_opt "GLOWSTICK_SHELL_TYPE" in
    match shell_opt with
    | None -> default
    | Some s -> match s with
        | "zsh"  -> (fun x -> "%{" ^ x ^ "%}")
        | "bash" -> (fun x -> "\\[" ^ x ^ "\\]")
        | _ -> default

let () =
    let _ = user_name () in
    let _ = host_name () in
    let cwd = cwd_label_abbreviated () in
    let git = git_info () in
    let nix = detect_nix_shell () in

    let esc = get_esc () in

    let ret = decorate ("" ^ cwd)
        |> foreground (Hex "0xE9E8D9")
        |> append_to_ansi "" esc in
    let ret = match git with
        | NotInGitRepo -> ret
        | _ -> decorate (string_of_git git)
            |> foreground (Hex "0xC9DF9E")
            |> append_to_ansi (ret ^ " on ") esc in
    let ret = match nix with
        | NotInNixShell -> ret
        | _ -> decorate (string_of_nix nix)
            |> foreground Magenta
            |> append_to_ansi (ret ^ ", ") esc in
    let ret = decorate "\u{ec07} "
        |> foreground White
        |> append_to_ansi ret esc in
    print_string ret;
