open Utils

let user_name () =
  let uid = Unix.getuid () in
  let pw = Unix.getpwuid uid in
  pw.Unix.pw_name

let host_name () = Unix.gethostname ()

let cwd () =
  let cwd = Sys.getcwd () in
  match Sys.getenv_opt "HOME" with
  | Some home -> (
      match remove_prefix home cwd with Some path -> "~" ^ path | None -> cwd)
  | _ -> cwd

let cwd_basename () =
  let cwd = Sys.getcwd () in
  match Sys.getenv_opt "HOME" with
  | Some home when cwd = home -> "~"
  | _ -> Filename.basename cwd

let cwd_abbreviated () =
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
      else abbreviate_segments (split_on_slash cwd)

type nix_shell_type = NotInNixShell | Pure | Impure | Unknown

let detect_nix_shell () =
  match Sys.getenv_opt "IN_NIX_SHELL" with
  | Some "pure" -> Pure
  | Some "impure" -> Impure
  | Some _ -> Unknown
  | None -> (
      match Sys.getenv_opt "PATH" with
      | Some haystack ->
          if contains_substring "/nix/store" haystack then Unknown
          else NotInNixShell
      | None -> NotInNixShell)

type git_repo_type = NotInGitRepo | Branch of string

let rec find_dotgit_dir (cur : string) =
  let dotgit = Filename.concat cur ".git" in
  if Sys.file_exists dotgit then Some (cur, dotgit)
  else
    let parent = Filename.dirname cur in
    if parent = cur then None else find_dotgit_dir parent

let resolve_git_dir (work_root : string) (dotgit : string) =
  if Sys.is_directory dotgit then Some dotgit
  else
    (* .git is a file with a pointer to real gitdir *)
    match read_file dotgit with
    | Ok content -> (
        let content = String.trim content in
        match remove_prefix "gitdir: " content with
        | Some p ->
            let p = String.trim p in
            let resolved =
              if Filename.is_relative p then Filename.concat work_root p else p
            in
            Some resolved
        | None -> None)
    | Error _ -> None

let branch_name_of_head (head_content : string) =
  let head = String.trim head_content in
  if String.starts_with ~prefix:"ref: " head then
    let refpath = String.sub head 5 (String.length head - 5) |> String.trim in
    match remove_prefix "refs/heads/" refpath with
    | Some b -> b
    | None -> (
        match remove_prefix "refs/" refpath with Some b -> b | None -> refpath)
  else
    let n = min 7 (String.length head) in
    "detached@" ^ String.sub head 0 n

let git_info () =
  match find_dotgit_dir (Sys.getcwd ()) with
  | None -> NotInGitRepo
  | Some (work_root, dotgit) -> (
      match resolve_git_dir work_root dotgit with
      | None -> NotInGitRepo
      | Some gitdir -> (
          let head_path = Filename.concat gitdir "HEAD" in
          match read_file head_path with
          | Ok s -> Branch (branch_name_of_head s)
          | Error _ -> NotInGitRepo))

let string_of_git = function NotInGitRepo -> "" | Branch b -> b

let string_of_nix = function
  | NotInNixShell -> ""
  | Pure -> "pure"
  | Impure -> "impure"
  | Unknown -> "unknown"

let time () =
  let tm = Unix.localtime (Unix.time ()) in
  Printf.sprintf "%02d:%02d:%02d" tm.tm_hour tm.tm_min tm.tm_sec

type status = Success | Fail

let last_status () =
  match Sys.getenv_opt "__GLOWSTICK_LAST_STATUS" with
  | Some s -> ( match int_of_string s with 0 -> Success | _ -> Fail)
  | _ -> Fail
