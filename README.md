# nshell

**A modern, fish-inspired interactive shell written in Common Lisp.**

[![CI](https://github.com/takeokunn/nshell/actions/workflows/ci.yml/badge.svg)](https://github.com/takeokunn/nshell/actions/workflows/ci.yml)
[![License: MIT](https://img.shields.io/badge/License-MIT-blue.svg)](./LICENSE)
[![Built with Nix](https://img.shields.io/badge/built%20with-nix-5277C3.svg?logo=nixos&logoColor=white)](https://nixos.org)

nshell is an interactive shell that puts the *interactive* experience first:
real-time syntax highlighting, history-aware autosuggestions, fish-style
abbreviations, and a fast, context-aware completion engine — all built on a
clean, test-driven Common Lisp core (4,900+ checks) and a reproducible Nix
build.

> **Status: early development (0.1.x).** The interactive editor and core
> pipeline execution are solid and heavily tested. The shell *language* is a
> growing subset of POSIX/fish semantics — see [Roadmap](#roadmap) for what is
> and isn't supported yet. nshell is usable as a daily interactive shell for
> common workflows; it is not yet a drop-in `/bin/sh` replacement for scripts.

---

## Highlights

- **Syntax highlighting** as you type — commands, strings, operators, and paths
  are colorized live.
- **Autosuggestions** from your history, fish-style, accepted with `→` / `Ctrl-F`.
- **Abbreviations** (`abbr`) that expand inline as you type — keep your muscle
  memory, type less.
- **Context-aware completion** — a knowledge base of commands/flags plus
  filesystem completion, with common-prefix `Tab` extension and a candidate menu.
- **Rich line editing** — Emacs keybindings, kill-ring & yank, multi-level
  undo/redo, multiline editing, and incremental history search (`Ctrl-R`).
  Optional **vi key bindings** (`NSHELL_VI_MODE=1`): normal-mode motions,
  operators (`dd`, `cw`, …), and insert/append.
- **Configurable prompt** — hostname, working directory, git branch/dirty
  status, command duration, and exit code, with theming.
- **Job control** — background jobs (`&`), `jobs`, `fg`, `bg`, `disown`.
- **Pipelines & redirection** — `|`, `>`, `>>`, `<`, logical `&&` / `||`, and
  command sequencing.
- **Control flow & functions** — `if`, `for`, `while`, `switch`, `begin`/`end`,
  and user-defined `function`s.
- **Reproducible build** — a single statically-dumped SBCL image via Nix;
  `nix run` and you're in.

## Quick start

With [Nix](https://nixos.org/download) (flakes enabled), run nshell without
installing anything:

```sh
nix run github:takeokunn/nshell
```

Or build a binary into your profile:

```sh
nix profile install github:takeokunn/nshell
nshell
man nshell   # the manual page is installed alongside the binary
```

### One-off command

```sh
nshell -c 'echo hello | string upper'
```

### CLI

```
Usage: nshell [--help] [--version] [-c COMMAND]

Without arguments, nshell starts an interactive shell when stdin is a terminal
and reads batch input from stdin otherwise.
With -c/--command, nshell executes COMMAND once in batch mode.
```

## Building from source

nshell builds with [SBCL](http://www.sbcl.org/) and ASDF. The supported and
tested path is Nix:

```sh
git clone https://github.com/takeokunn/nshell
cd nshell
nix build            # produces ./result/bin/nshell
nix flake check      # build + full test suite + smoke tests
nix develop          # dev shell with SBCL + FiveAM
```

Inside `nix develop`, you can load the system into a REPL:

```lisp
(asdf:load-system :nshell)
(nshell:main)
```

## Built-in commands

`alias`, `abbr`, `bg`, `cd`, `complete`, `contains`, `count`, `disown`, `echo`,
`exec`, `exit`, `export`, `false`, `fg`, `function`, `help`, `history`, `jobs`,
`ls`, `not`, `pwd`, `read`, `seq`, `set`, `source`, `string`, `test`, `true`,
`type`, `which`.

Run `help` inside nshell for details.

## Architecture

nshell follows a domain-driven, layered design. Each layer depends only on the
layers beneath it:

```
src/
├── domain/          Pure shell logic: parsing, expansion, completion,
│                    history, prompting, job-control — no I/O.
├── application/     Use cases: builtins, pipeline execution, job management.
├── infrastructure/  ACLs over the OS: syscalls, PTY, signals, terminal I/O,
│                    persistence. SBCL-specific code is isolated here.
└── presentation/    The REPL, line editor (input-state reducer), rendering,
                     highlighting, autosuggestions, completion UI.
```

The REPL is structured as a **continuation-passing / trampoline loop**: each
keystroke runs a pure reducer over an immutable `input-state`, and rendering is
derived from that state. This keeps the interactive core deterministic and
unit-testable without a terminal.

## Testing

The suite uses [FiveAM](https://github.com/lispci/fiveam) and runs as part of
`nix flake check`:

```sh
nix build .#checks.$(nix eval --impure --raw --expr builtins.currentSystem).test
```

Unit, integration, property-based, and end-to-end (PTY) tests live under
`tests/`.

## Roadmap

nshell is converging on world-class interactive-shell parity. Near-term focus:

- **Shell language depth** — here-docs/here-strings and richer list variables.
  (Quoting, parameter expansion incl. patterns, arithmetic `$((...))`, brace
  expansion, command substitution `$(...)`/`(...)`, fd redirections
  `2>`/`2>&1`/`&>`, and function arguments via `$argv`/`$argv[N]` are done.)
- **Job control hardening** — robust foreground process-group handling so
  `Ctrl-C` / `Ctrl-Z` reliably interrupt and suspend pipelines.
- **Editor parity** — visual selection and numeric arguments (vi-mode and
  Emacs bindings are done).
- **Distribution** — nixpkgs, Homebrew, and prebuilt release binaries.

See [CHANGELOG.md](./CHANGELOG.md) for released changes.

## Contributing

Contributions are welcome. Please run `nix flake check` before opening a pull
request — CI runs the same checks on Linux and macOS. Bug reports and feature
requests are welcome via GitHub Issues.

## License

[MIT](./LICENSE) © the nshell authors.
