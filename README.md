# Glowstick

A simple, personal terminal-prompt written in OCaml.

Glowstick is for my personal use, which means features are added at my leisure
and breaking changes are common. Take this project with a grain of salt.

With that being said, feel free to reach out for suggestions or wanted
features.

## Usage
Initialize via eval (choose a theme):

- zsh (`~/.zshrc`):
```
eval "$(/path/to/executable init zsh tomita)"
```

- bash (`~/.bashrc`):
```
eval "$(/path/to/executable init bash bobthefish)"
```

Replace `/path/to/executable` with the full path or a command in your `PATH`,
and replace the theme (e.g. `macos`, `tomita`) with your preferred theme.

Notes:
- zsh uses native `PROMPT` and `RPROMPT` for stable left/right prompts.
- bash sets `PS1` via `PROMPT_COMMAND` and does not provide a native right
prompt. The left prompt is rendered with correct escape-wrapping to avoid
redraw issues.

Manual rendering (useful for testing):
- Left prompt:  ``__GLOWSTICK_SHELL_TYPE=zsh /path/to/executable prompt left tomita``
- Right prompt: ``__GLOWSTICK_SHELL_TYPE=zsh /path/to/executable prompt right tomita``

`__GLOWSTICK_SHELL_TYPE` can be `zsh` or `bash` to ensure escape sequences are
correctly wrapped for the target shell. The init scripts set this
automatically.

### Themes
Available themes: (some are oh-my-fish inspired)
- `robby`
- `current`
- `macos`
- `sushi`
- `tomita`

## Install
If you use nix, simply run:
`$ nix run` or `$ nix build`
while in the projects directory. This will build the project and will put the
executable in `result/bin/main`.

If you do not use nix, then install dune/opam (which is Ocaml's package
manager) and build the project via `$ dune build` or `$ dune build main`. This
will put the executable at `_build/install/default/bin/main`.

You can also run the project via `$ dune exec main`.

## Developing
Internals use a small decorator DSL for ANSI styling. The current API centers
on:
- `Decorated_string.text : string -> Decorated_string.t`
- `Decorated_string.render : ?esc:(string -> string) -> Decorated_string.t ->
string`

Helpers like `foreground`, `background`, `bold`, `underlined`, and `italic`
compose with `text`. The optional `esc` argument wraps sequences for your shell
(`Utils.get_esc ()` is used by default).
