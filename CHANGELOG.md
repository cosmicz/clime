# Changelog

## Unreleased (0.2.0)

### Added

- Ancestor option propagation: options from any ancestor (app, group) are
  now available to all descendant commands, with a "Global Options:" section
  in help output
- Two-pass parse with `:setup` hook: app-level initialization runs after
  pass-1 (argv parsed) but before pass-2 (dynamic choices validated, env/defaults applied).
  Enables config loading that influences lazy `:choices` and `:default` functions.
- `clime-params-plist`: convert context params to keyword plist, eliminating
  `clime-let` + plist reconstruction boilerplate
- `clime-parse-finalize`: public API for explicit pass-2 control

### Changed

- Renamed `:group` option slot to `:category` (avoids confusion with `clime-group`)
- Static `:choices` (literal lists) validated in pass 1; dynamic `:choices`
  (functions) deferred to pass 2

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
