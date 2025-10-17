let build_left (theme : string) () = match Themes.match_to_theme theme with
    | Some t -> (fst t) ()
    | None -> "Unknown theme was specified."

let build_right (theme : string) () = match Themes.match_to_theme theme with
    | Some t -> (snd t) ()
    | None -> "Unknown theme was specified."

let zsh_init_script theme () =
    let self = Sys.argv.(0) in ""
    ^  "# glowstick init for zsh                                          \n"
    ^  "prompt_glowstick_precmd() {                                       \n"
    ^  "  local left right                                                \n"
    ^ ("  left=\"$(GLOWSTICK_SHELL_TYPE=zsh " ^ self ^ " prompt left " ^ theme ^ ")\"   \n")
    ^ ("  right=\"$(GLOWSTICK_SHELL_TYPE=zsh " ^ self ^ " prompt right " ^ theme ^ ")\" \n")
    ^  "  PROMPT=\"$left\"                                                \n"
    ^  "  RPROMPT=\"$right\"                                              \n"
    ^  "}                                                                 \n"
    ^  "autoload -Uz add-zsh-hook\n"
    ^  "add-zsh-hook precmd prompt_glowstick_precmd\n"

let bash_init_script theme () =
    let self = Sys.argv.(0) in ""
    ^  "# glowstick init for bash                                         \n"
    ^  "__glowstick_prompt_command() {                                    \n"
    ^ ("  PS1=\"$(GLOWSTICK_SHELL_TYPE=bash " ^ self ^ " prompt left " ^ theme ^ ")\"   \n")
    ^  "}                                                                 \n"
    ^  "if [[ -n \"$PROMPT_COMMAND\" ]]; then                             \n"
    ^  "  PROMPT_COMMAND=\"__glowstick_prompt_command; $PROMPT_COMMAND\"  \n"
    ^  "else                                                              \n"
    ^  "  PROMPT_COMMAND=__glowstick_prompt_command                       \n"
    ^  "fi                                                                \n"

let () =
    let argv = Array.to_list Sys.argv in
    match argv with
    | _ :: [ "init"; "zsh"; theme ] -> print_string (zsh_init_script theme ())
    | _ :: [ "init"; "bash"; theme] -> print_string (bash_init_script theme ())
    | _ :: [ "prompt"; "left"; theme ] -> print_string (build_left theme ())
    | _ :: [ "prompt"; "right"; theme ] -> print_string (build_right theme ())
    | _ -> (* Fallback *) print_string "glowstick has failed: Invalid arguments."
