# Glowstick

A simple, personal terminal-prompt written in OCaml.

Glowstick is for my personal use, which means features are added at my leisure
and breaking changes are common. Take this project with a grain of salt.

With that being said, feel free to reach out for suggestions or wanted
features.

## Usage
Initialize via eval:

- zsh (`~/.zshrc`):
```
eval "$(/path/to/executable init zsh)"
```

- bash (`~/.bashrc`):
```
eval "$(/path/to/executable init bash)"
```

Replace `/path/to/executable` with the full path or the command name if it is in your `PATH`.

Notes:
- zsh uses native `PROMPT` and `RPROMPT` for stable left/right prompts.
- bash sets `PS1` via `PROMPT_COMMAND` and does not provide a native right prompt.
  The left prompt is rendered with correct escape-wrapping to avoid redraw issues.

## Install
If you use nix, simply run:
`$ nix run` or `$ nix build`
while in the projects directory. This will build the project and will put the
executable in `result/bin/main`.

If you do not use nix, then install dune/opam (which is Ocaml's package manager) and
build the project via `$ dune build main`. This will put the executable at
`_build/install/default/bin/main`.

You can also run the project via `$ dune exec main`.
