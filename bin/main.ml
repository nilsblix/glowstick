open Decorated_string

let build_left (theme : string) () =
  match Themes.match_to_theme theme with
  | Some t -> Utils.escape_prompt_literals ((fst t) ())
  | None ->
      text "(!!) Unknown left theme: "
      |> foreground Red |> render None |> Utils.escape_prompt_literals

let build_right (theme : string) () =
  match Themes.match_to_theme theme with
  | Some t -> Utils.escape_prompt_literals ((snd t) ())
  | None ->
      text "(!!) Unknown right theme"
      |> foreground Red |> render None |> Utils.escape_prompt_literals

let shell_quote (s : string) =
  let buf = Buffer.create (String.length s + 2) in
  Buffer.add_char buf '\'';
  String.iter
    (function
      | '\'' -> Buffer.add_string buf "'\\''" | c -> Buffer.add_char buf c)
    s;
  Buffer.add_char buf '\'';
  Buffer.contents buf

let prompt_command shell side theme =
  "env __GLOWSTICK_SHELL_TYPE=" ^ shell_quote shell
  ^ " __GLOWSTICK_LAST_STATUS=$last_status "
  ^ shell_quote Sys.argv.(0)
  ^ " prompt " ^ shell_quote side ^ " " ^ shell_quote theme

let zsh_init_script theme () =
  let left = prompt_command "zsh" "left" theme in
  let right = prompt_command "zsh" "right" theme in
  "" ^ "# glowstick init for zsh                                      \n"
  ^ "prompt_glowstick_precmd() {                                      \n"
  ^ "  local last_status=$?                                           \n"
  ^ "  local left right                                               \n"
  ^ ("  left=\"$(" ^ left ^ ")\"                                      \n")
  ^ ("  right=\"$(" ^ right ^ ")\"                                    \n")
  ^ "  PROMPT=\"$left\"                                               \n"
  ^ "  RPROMPT=\"$right\"                                             \n"
  ^ "}                                                                \n"
  ^ "autoload -Uz add-zsh-hook\n"
  ^ "add-zsh-hook precmd prompt_glowstick_precmd\n"

let bash_init_script theme () =
  let left = prompt_command "bash" "left" theme in
  "" ^ "# glowstick init for bash                                     \n"
  ^ "__glowstick_prompt_command() {                                   \n"
  ^ "  local last_status=$?                                           \n"
  ^ ("  PS1=\"$(" ^ left ^ ")\"                                       \n")
  ^ "}                                                                \n"
  ^ "if [[ -n \"$PROMPT_COMMAND\" ]]; then                            \n"
  ^ "  PROMPT_COMMAND=\"__glowstick_prompt_command; $PROMPT_COMMAND\" \n"
  ^ "else                                                             \n"
  ^ "  PROMPT_COMMAND=__glowstick_prompt_command                      \n"
  ^ "fi                                                               \n"

let fish_init_script theme () =
  let left = prompt_command "fish" "left" theme in
  let right = prompt_command "fish" "right" theme in
  "" ^ "# glowstick init for fish\n" ^ "function fish_prompt\n"
  ^ "  set -l last_status $status\n"
  ^ ("  " ^ left ^ "\n")
  ^ "end\n" ^ "function fish_right_prompt\n" ^ "  set -l last_status $status\n"
  ^ ("  " ^ right ^ "\n")
  ^ "end\n"

let () =
  let argv = Array.to_list Sys.argv in
  match argv with
  | _ :: [ "zsh"; theme ] -> print_string (zsh_init_script theme ())
  | _ :: [ "bash"; theme ] -> print_string (bash_init_script theme ())
  | _ :: [ "fish"; theme ] -> print_string (fish_init_script theme ())
  | _ :: [ "prompt"; "left"; theme ] -> print_string (build_left theme ())
  | _ :: [ "prompt"; "right"; theme ] -> print_string (build_right theme ())
  | _ -> (* Fallback *) print_string "glowstick has failed: Invalid arguments."
