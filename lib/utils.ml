let remove_prefix prefix s =
  let len_prefix = String.length prefix in
  if String.starts_with ~prefix s then
    Some (String.sub s len_prefix (String.length s - len_prefix))
  else None

let contains_substring (needle : string) (haystack : string) =
  let n = String.length needle in
  let h = String.length haystack in
  if n = 0 then true
  else
    let rec scan i =
      if i + n > h then false
      else if String.sub haystack i n = needle then true
      else scan (i + 1)
    in
    scan 0

let read_file (path : string) =
  try
    let ic = open_in_bin path in
    let s =
      Fun.protect
        ~finally:(fun () -> close_in_noerr ic)
        (fun () -> really_input_string ic (in_channel_length ic))
    in
    Ok s
  with e -> Error (Printexc.to_string e)

let split_on_slash (s : string) =
  let len = String.length s in
  let rec loop i start acc =
    if i >= len then
      let seg = String.sub s start (len - start) in
      List.rev (seg :: acc)
    else if s.[i] = '/' then
      let seg = String.sub s start (i - start) in
      loop (i + 1) (i + 1) (seg :: acc)
    else loop (i + 1) start acc
  in
  if len = 0 then [] else loop 0 0 []

let abbreviate_segments (segments : string list) =
  match List.rev segments with
  | [] -> ""
  | last :: rest_rev ->
      let rest = List.rev rest_rev in
      let abbrev_seg s =
        if s = "" then ""
        else
          let len = String.length s in
          let is_alpha c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') in
          let rec find i =
            if i >= len then 1
            else if is_alpha s.[i] then i + 1
            else find (i + 1)
          in
          let n = find 0 in
          String.sub s 0 n
      in
      let abbrev_rest = List.map abbrev_seg rest in
      String.concat "/" (abbrev_rest @ [ last ])

let get_esc () =
  let default = fun x -> x in
  let shell_opt = Sys.getenv_opt "__GLOWSTICK_SHELL_TYPE" in
  match shell_opt with
  | None -> default
  | Some s -> (
      match s with
      | "zsh" -> fun x -> "%{" ^ x ^ "%}"
      | "bash" -> fun x -> "\\[" ^ x ^ "\\]"
      | _ -> default)

let visible_length (s : string) : int =
  let len = String.length s in
  let rec find_end_zsh i =
    if i + 1 >= len then len
    else if s.[i] = '%' && s.[i + 1] = '}' then i + 2
    else find_end_zsh (i + 1)
  in
  let rec find_end_bash i =
    if i + 1 >= len then len
    else if s.[i] = '\\' && s.[i + 1] = ']' then i + 2
    else find_end_bash (i + 1)
  in
  let rec skip_ansi j =
    if j >= len then len
    else if s.[j] >= '@' && s.[j] <= '~' then j + 1
    else skip_ansi (j + 1)
  in
  let rec loop i acc =
    if i >= len then acc
    else if s.[i] = '%' && i + 1 < len && s.[i + 1] = '{' then
      loop (find_end_zsh (i + 2)) acc
    else if s.[i] = '\\' && i + 1 < len && s.[i + 1] = '[' then
      loop (find_end_bash (i + 2)) acc
    else if s.[i] = '\x1b' then loop (skip_ansi (i + 1)) acc
    else loop (i + 1) (acc + 1)
  in
  loop 0 0
