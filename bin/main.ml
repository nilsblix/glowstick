let build_left () =
    Themes.Tomita.left ()

let build_right () =
    Themes.Tomita.right ()

let zsh_init_script () =
    let self = Sys.argv.(0) in ""
    ^  "# glowstick init for zsh                                          \n"
    ^  "prompt_glowstick_precmd() {                                       \n"
    ^  "  local left right                                                \n"
    ^ ("  left=\"$(GLOWSTICK_SHELL_TYPE=zsh " ^ self ^ " prompt left)\"   \n")
    ^ ("  right=\"$(GLOWSTICK_SHELL_TYPE=zsh " ^ self ^ " prompt right)\" \n")
    ^  "  PROMPT=\"$left\"                                                \n"
    ^  "  RPROMPT=\"$right\"                                              \n"
    ^  "}                                                                 \n"
    ^  "autoload -Uz add-zsh-hook\n"
    ^  "add-zsh-hook precmd prompt_glowstick_precmd\n"

let bash_init_script () =
    let self = Sys.argv.(0) in ""
    ^  "# glowstick init for bash                                         \n"
    ^  "__glowstick_prompt_command() {                                    \n"
    ^ ("  PS1=\"$(GLOWSTICK_SHELL_TYPE=bash " ^ self ^ " prompt left)\"   \n")
    ^  "}                                                                 \n"
    ^  "if [[ -n \"$PROMPT_COMMAND\" ]]; then                             \n"
    ^  "  PROMPT_COMMAND=\"__glowstick_prompt_command; $PROMPT_COMMAND\"  \n"
    ^  "else                                                              \n"
    ^  "  PROMPT_COMMAND=__glowstick_prompt_command                       \n"
    ^  "fi                                                                \n"

let () =
    let argv = Array.to_list Sys.argv in
    match argv with
    | _ :: [ "init"; "zsh" ] -> print_string (zsh_init_script ())
    | _ :: [ "init"; "bash" ] -> print_string (bash_init_script ())
    | _ :: [ "prompt"; "left" ] -> print_string (build_left ())
    | _ :: [ "prompt"; "right" ] -> print_string (build_right ())
    | _ -> (* Fallback *) print_string "glowstick has failed: Invalid arguments."
