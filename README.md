# Glowstick

A simple, personal terminal-prompt written in OCaml.

Glowstick is for my personal use, which means features are added at my leisure
and breaking changes are common. Take this project with a grain of salt.

With that being said, feel free to reach out for suggestions or wanted
features.

## Usage
The program is an executable which simply prints out a terminal-prompt.
Here are some common ways, in common shells, to apply glowstick:
```zshrc
function precmd () {
    prompt=$(GLOWSTICK_SHELL_TYPE=zsh ~/path/to/executable)
}
```

```bashrc
PS1=$(GLOWSTICK_SHELL_TYPE=bash ~/path/to/executable)
```

These are currently the only two supported shells, but create a PR
if you want to add other shells. It should be straightforward, in theory...
as the only difference between the shells are how ANSI-escape codes are escaped.
Zsh uses a combination of `%` and `{` while bash uses multiple `[`.

## Install
If you use nix, simply run:
`$ nix run` or `$ nix build`
while in the projects directory. This will build the project and will put the
executable in `result/bin/main`.

If you do not use nix, then install dune/opam (which is Ocaml's package manager) and
build the project via `$ dune build main`. This will put the executable at
`_build/install/default/bin/main`.

You can also run the project via `$ dune exec main`.
