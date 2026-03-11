# Changelog

## Unreleased

### Added

- `clime-alias-for` DSL form: declare a command as an alias for a nested
  command path.  Copies args, options, and handler from the target at init
  time, eliminating boilerplate when exposing nested commands at a higher
  level.  Supports `:help`, `:aliases`, `:hidden`, `:category` overrides,
  transitive resolution, and circular chain detection.
- Terminal-width-aware help text wrapping: help descriptions, epilog text,
  and table right-columns now wrap at the terminal width.  Auto-detects
  from `COLUMNS` env var (fallback 80, minimum 40).  Override with
  `clime-help-width` variable.

## 0.2.0 — 2026-03-10

### Added

- Ancestor option propagation: options from any ancestor (app, group) are
  now available to all descendant commands, with a "Global Options:" section
  in help output
- Two-pass parse with `:setup` hook: app-level initialization runs after
  pass-1 (argv parsed) but before pass-2 (dynamic choices validated, env/defaults applied).
  Enables config loading that influences lazy `:choices` and `:default` functions.
- `clime-params-plist`: convert context params to keyword plist, eliminating
  `clime-let` + plist reconstruction boilerplate
- `:conform` slot on options and args: pass-2 value conformer that runs
  after env vars are applied but before defaults.  Validates and
  transforms external input (CLI + env); defaults skip conforming.
  Pipeline: type → `:coerce` → choices → env → `:conform` → defaults.
- `clime-defopt` and `clime-defarg`: reusable parameter templates.
  Define option/arg slot defaults once, inherit via `:from` in any
  command.  Explicit slot values override the template.
- `clime-parse-finalize`: public API for explicit pass-2 control
- `:separator` slot on options: split a single value into a list
  (e.g. `--tags=a,b,c` with `:separator ","` yields `("a" "b" "c")`)
- `:category` slot on commands, groups, and options: group items under
  named sections in help output (e.g. `Admin:`, `I/O:`)

- `scaffold` command: auto-detect `clime-app` symbol in a file and insert
  `;;; Entrypoint:` boilerplate with `clime-main-script-p` guard.  Uses
  `(provide 'FEATURE)` for the guard symbol when present, falls back to the
  app symbol.
- `quickstart` command: composes `scaffold` + `init` in one shot.  Auto-detects
  `CLIME_MAIN_APP` from the `clime-app` form (explicit `-e` overrides).
  Accepts all of `init`'s flags (`--self-dir`, `-R`, `--standalone`, etc.).

- `init --self-dir` and `init --rel-load-path` now resolve symlinks at runtime
  via `realpath`, so relative load paths work correctly when the script is
  invoked through a symlink

### Changed

- Shebang tag changed from `# clime:X.Y.Z` to `# clime-sh!:vN`, separating
  the shebang format version from the library version
- `init` refuses to downgrade a newer shebang format version (use `--force`
  to override); detects and upgrades legacy `clime:X.Y.Z` tags automatically
- Renamed `:group` option slot to `:category` (avoids confusion with `clime-group`)
- Help renderer rewritten as a collect-and-render pipeline, improving inline
  group handling and enabling category-based section grouping
- Static `:choices` (literal lists) validated in pass 1; dynamic `:choices`
  (functions) deferred to pass 2
- `bundle --main` now takes a file path (was a symbol name); the main file's
  code is wrapped in a `clime-main-script-p` guard and placed in an
  `;;; Entrypoint:` section
- `bundle` strips `;;; Entrypoint:` sections from source files instead of
  using regex to remove `(clime-run-batch ...)` forms
- `bundle` errors if source files contain `(clime-run-batch ...)` in their
  library section (must be below `;;; Entrypoint:` marker)

## 0.1.1 — 2026-03-09

### Added

- `:choices` and `:coerce` slots for option/arg value validation
- `--help` before subcommand now works (`--help install` = `install --help`)
- Help epilog support, `:doc` alias for `:help`, multiline truncation in listings
- Help hint includes full command path on errors
- `clime-version` constant
- Version-tagged `init` headers with automatic update support
- Rest args collector recognizes known options (boolean and value)
- `init --self-dir` adds runtime `$(dirname "$0")` load path to shebang
- `init --rel-load-path` (`-R`) adds load paths relative to script dir at runtime
- `:choices` now accepts a function for lazy evaluation at parse/help time
- Inline groups (`:inline t`): promote a group's children to parent level for dispatch and help

### Fixed

- Shebang now enables `lexical-binding` via read/eval loop (the polyglot
  `":"` prefix prevented Emacs from recognizing the `-*-` cookie)

### Changed

- Renamed `clime-app.el` to `clime-make.el` (clearer purpose)
- Comprehensive README rewrite with full feature documentation

## 0.1.0 — 2026-03-06

Initial release.

- Declarative DSL (`clime-app`, `clime-command`, `clime-group`, `clime-option`, `clime-arg`)
- Single-pass CLI argument parser
- Auto-generated `--help` and `--version`
- Boolean flags, count flags, multiple values, optional args, rest args
- Environment variable fallback with auto-derived names
- Stdin reading via `-` sentinel
- JSON output mode (`--json`)
- Type coercion (string, integer, number)
- Nested command groups
- `bundle` command for single-file distribution
- `init` command for polyglot shebang headers
