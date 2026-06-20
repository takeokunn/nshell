# Changelog

All notable changes to nshell are documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Added
- POSIX parameter-expansion operators inside `${...}`: `${VAR:-default}`,
  `${VAR-default}`, `${VAR:=default}`, `${VAR:+alt}`, `${VAR+alt}`,
  `${VAR:?msg}`, and length `${#VAR}`. The default/alternate word is itself
  variable-expanded. (The `:=` assignment side effect is not yet performed.)
- `CONTRIBUTING.md`, `SECURITY.md`, GitHub issue templates, and a pull-request
  template.
- `LICENSE` file (MIT) at the repository root.
- GitHub Actions CI (`nix flake check` — build + full test suite + smoke tests)
  on a Linux + macOS matrix, triggered on pushes, pull requests, and tags.
- GitHub Actions release workflow that builds per-platform binaries and uploads
  tarballs with SHA-256 checksums to GitHub Releases on `v*` tags.
- `meta` attributes (license, homepage, platforms, mainProgram) on the Nix
  package, and `:homepage` / `:source-control` to the ASDF system definition.
- This `CHANGELOG.md` and a project `README.md`.

### Changed
- Double-quoted strings now follow POSIX semantics: variables are expanded but
  globbing and word-splitting are suppressed, so `"$VAR"` expands while `"*"`
  stays literal. Arguments now carry a three-way quote style
  (unquoted / `:single` / `:double`) through the tokenizer, parser, and
  expander instead of a single "is-literal" boolean.

### Fixed
- Made the `repl-clear-screen` rendering test hermetic: it now pins the prompt
  width and terminal size so the asserted rendered-line count no longer depends
  on the ambient working directory (the default prompt renders the cwd, which is
  a long path inside the build sandbox). The full suite is green (4912 checks).

## [0.1.0]

Initial development version: fish-inspired interactive shell in Common Lisp
(SBCL) with a CPS/trampoline REPL, syntax highlighting, autosuggestions,
abbreviations, completion engine, kill-ring/undo, history search, job control,
and a Nix-based reproducible build.
