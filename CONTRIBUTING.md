# Contributing to nshell

Thanks for your interest in improving nshell! This document explains how to set
up your environment, the expectations for changes, and how to get a pull
request merged.

## Development environment

nshell is built with [SBCL](http://www.sbcl.org/) and ASDF, and the supported,
reproducible toolchain is [Nix](https://nixos.org/download) with flakes enabled.

```sh
git clone https://github.com/takeokunn/nshell
cd nshell
nix develop          # SBCL + FiveAM dev shell
nix build            # build ./result/bin/nshell
nix flake check      # build + full test suite + smoke tests
```

Inside `nix develop` you can iterate in a REPL:

```lisp
(asdf:load-system :nshell)        ; load the shell
(asdf:test-system :nshell/test)   ; run the test suite
(nshell:main)                     ; start the shell
```

## Architecture

nshell follows a layered, domain-driven design (see the README for the diagram).
Please keep dependencies pointing inward:

- `domain/` is pure shell logic with **no I/O**. New parsing, expansion,
  completion, or history logic belongs here and should be unit-testable without
  a terminal.
- `infrastructure/` isolates all OS/SBCL-specific code (syscalls, PTY, signals,
  terminal). Guard implementation-specific code with `#+sbcl` where relevant.
- `application/` holds use cases (builtins, pipeline execution, job management).
- `presentation/` is the REPL, line editor, and rendering.

## Making changes

1. **Branch** off `main`.
2. **Add tests.** Every behavior change should come with FiveAM tests under
   `tests/` (unit, integration, property-based, or e2e as appropriate). Prefer
   testing pure domain logic directly.
3. **Keep tests hermetic.** Tests must not depend on the ambient working
   directory, terminal size, or environment. Use the provided fixtures (e.g.
   `with-stable-repl-prompt`, `with-fixed-terminal-size`) for rendering tests.
4. **Run `nix flake check`** locally — CI runs the same checks on Linux and
   macOS, and a green check is required to merge.
5. **Update `CHANGELOG.md`** under `[Unreleased]`.
6. **Match the surrounding style** — naming, comment density, and idiom.

## Commit & PR conventions

- Write focused commits with clear messages (imperative mood).
- Open a pull request describing the motivation and the user-visible effect.
- Link any related issue.

## Reporting bugs & requesting features

Please use GitHub Issues. For bugs, include the nshell version
(`nshell --version`), your OS, and a minimal reproduction.

By contributing, you agree that your contributions are licensed under the
project's [MIT License](./LICENSE).
