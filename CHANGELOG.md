# Changelog

## Unreleased

## 0.7.0 — 2026-04-20

### Added

- **`clime-serve` module**: HTTP surface for clime apps.  The app tree IS
  the router — URL path segments resolve commands via
  `clime-group-find-child`, and non-matching segments are consumed as
  positional args (supporting path params like `/agent/:id/start`).
  Mirrors `clime-invoke`'s dispatch shape: setup/config runs once at
  startup (two-pass), the tree is deep-copied per request, values are
  seeded from query string (GET) or JSON body (POST) directly into the
  values map.  Exit code → HTTP status mapping (0→200, 2→400, 1→500).
  A `/_routes` endpoint introspects the command tree.  `web-server` is
  vendored under `lib/`; no ELPA dependency.

- **`clime-make serve`**: new subcommand that loads a clime app file
  and exposes it over HTTP via `clime-serve`.  Accepts `--port` /
  `--host` / `--app` / `-L`.  When the file has a clime shebang, load
  paths (`-L $D`, `-L $D/rel`, absolute) and `CLIME_MAIN_APP` are
  parsed from the shebang automatically; `--no-auto-paths` disables this.

- **Path suffix format selection**: URL paths ending with `.json`,
  `.html`, etc. activate the matching `clime-output-format` when
  registered on the app.  The suffix is stripped before tree walking,
  and `clime-out--active-format` is bound for the handler.  Dispatch
  returns `:content-type` derived from the active format.  `_api`
  endpoints auto-default to JSON when the app declares a JSON format.
  Apps without output formats are unaffected — suffixes are literal.

- **Introspection API endpoints**: `/_api/meta`, `/_api/commands`, and
  `/_api/commands/PATH` provide structured introspection of the app tree.
  Endpoints are injected as hidden commands at serve startup — no
  special-case dispatch.  Output uses `clime-out` so the active output
  format drives encoding.  `/_routes` plain-text listing preserved.
  Rest-arg support in `clime-serve--walk-path` enables variable-depth
  paths like `/_api/commands/db/migrate`.

- **After-execute hooks**: new `:after-execute` slot on `clime-app`
  accepts a function or list of functions called after every handler
  execution with `(ctx exit-code duration-secs)`.  Fires across all
  surfaces (CLI, HTTP, interactive) via `clime-run--execute`.  Errors
  in hooks are caught and reported via `message`, never propagated.
  `clime-context` now includes a `start-time` slot (float-time) set
  before handler invocation, available to handlers for timeout/progress
  logic.

- **Config file providers**: new `:config` slot on `clime-app` accepts a
  factory function `(app result) → provider` called between pass-1 and
  pass-2 (after `:setup`).  The returned provider is a lookup function
  `(command-path param-name) → value` used to supply defaults from
  external config files.  Source precedence:
  `user > app > env > config > default > conform`.

- **Built-in providers**: `clime-config-json` and `clime-config-sexp`
  create providers from JSON and plist files respectively.  Both support
  hierarchical inheritance — a param defined at the root level is
  inherited by all commands, and commands can override it with
  command-specific values.

- **`clime-config.el` module**: new module providing the built-in
  provider creators and shared utilities.

- **`clime-nil-handler`**: new macro that wraps a handler so its return
  value is discarded (returns nil), preventing the runner from encoding
  it as output.  Accepts function references or inline bodies, like
  `clime-handler`.

- **`clime-parse-result-param`**: accessor for extracting a param from a
  parse result with an optional default.  Mirrors `clime-param` (for
  contexts), distinguishing nil-valued from absent.

- **`:immediate 'no-confirm`**: `clime-invoke` accepts `'no-confirm` as
  the `:immediate` value to skip `y-or-n-p` confirmation and run the
  handler directly when all params are pre-filled.

### Fixed

- **`clime-invoke` immediate confirmation**: `:immediate t` with all
  params pre-filled now renders the menu buffer before `y-or-n-p`, so
  the user sees the command state they're confirming.  The prompt also
  shows the full command path (e.g. "Run myapp install?").

### Internal

- **`clime-run-from-values`**: shared entry point for running a command
  handler from a pre-built values alist.  Consolidates the
  finalize → context → execute pipeline used by CLI, invoke, and serve
  surfaces.  Returns `(exit-code . output)`.

- **`clime--coerce-string-values`**: type coercion for string values
  from external sources (HTTP query params, JSON bodies).  Called in
  `clime-run-from-values` before finalize.  Uses type-only coercion
  (`clime--coerce-value`) to avoid double-applying `:coerce` transforms.

- **`clime-invoke` section renderers**: `clime-invoke--render-to-string`
  split into four focused renderers (`--render-options`, `--render-args`,
  `--render-children`, `--render-actions`) plus a compact orchestrator.

- **`clime-config.el` added to dist bundle**: the module was missing
  from `DIST_SRCS`, causing `dist/clime.el` to fail on `(require
  'clime-config)`.

## 0.6.1 — 2026-04-07

### Fixed

- **Shebang error handling**: the polyglot shebang now catches load-time
  errors (bad syntax, missing `require`) and prints them to stderr with
  exit code 1, instead of silently exiting 255.  Shebang format bumped
  to v2.

- **`clime-check-paired` values preservation**: the paired conformer
  returned nil when no members were set, wiping the entire values map
  during finalization.  Commands with `clime-zip` and positional
  arguments no longer lose their parsed values.

### Internal

- **`clime-usage-error` plist protocol**: signal data unified to
  `(MSG &rest PLIST)` with optional `:path` and `:params` keys.
  Readers use `plist-get` instead of positional access.  The parser's
  error enrichment preserves existing plist keys when adding `:path`.

## 0.6.0 — 2026-04-02

### Type system

- **Registry-based types**: `:type` now accepts type specs — symbols
  or S-expression lists — that resolve via a type registry.  Built-in
  types: `string`, `integer`, `number`, `boolean`, `json`, `file`,
  `directory`, `path` (short aliases: `str`, `int`, `num`, `bool`,
  `dir`).  Parameterized forms support constraints:
  `(integer :min 1 :max 65535)`, `(string :match "^[a-z]+$")`,
  `(file :must-exist t)`.  Composite types: `(member "json" "csv")`
  for string enums with completion, `(const "off")` for exact match,
  `(choice (integer :min 1) (const "off"))` for union types (first
  match wins).

- **`clime-deftype`**: define and register custom types.  The macro
  creates a constructor function and registers it — the type is
  immediately usable in `:type` specs.

- **Type-aware help and invoke**: CLI help appends the type description
  after help text — e.g., `(integer 1–65535)`.  Invoke prepends it in
  the desc column.  Redundancy suppression hides the hint when it
  duplicates choices.  Composite types provide `:choices` for invoke
  completion automatically.

- **Breaking: function `:type` removed**: `:type` no longer accepts a
  bare function.  Use `clime-deftype` to register a named type, or use
  `:coerce` for ad-hoc transforms.

### Invoke

- **`:key` slot**: options, arguments, commands, and groups now accept
  `:key` to set the preferred single-char key in the `clime-invoke`
  menu.  Overrides the auto-derived key (flag letter for options, first
  letter of name for commands).  Collisions fall back to auto-assignment.

- **`:ask` and `:immediate` keywords**: `clime-invoke` accepts `:ask`
  and `:immediate` for pre-menu prompting and immediate execution.
  `:ask t` prompts for all required params via minibuffer before showing
  the menu; `:ask '(param ...)` prompts for specific params.
  `:immediate t` runs the handler directly after prompting, bypassing
  the menu — enabling `clime-invoke` as a drop-in for interactive Emacs
  commands.  The menu is displayed during the ask phase with live value
  updates.

- **3-column layout**: menu reordered from Key|Desc|Value|Env to
  Key|Value|Desc for easier value scanning.  Compact choices format
  (`json|(csv)|html`), env-derived values (`($=/path)`) in value
  column, env annotation (`[$VAR]`) in description.

- **Desc column annotations**: required, multi, and type hints are
  prepended to the description — e.g.,
  `(required) (file existing) The .el file to set up`.

### Features

- **`:optional` keyword**: `clime-arg` and `clime-opt` accept
  `:optional` as the inverse of `:required`.  Mutually exclusive with
  `:required` (error if both present).

- **`clime-defopt` templates with `:type`**: parameter templates now
  support `:type`, with per-instance overrides via `:from`.

### Internal

- **Values map as single source of truth**: the runtime data carrier
  for both parse and invoke is now a values map — an alist of
  `(NAME :value V :source S)` entries with optional `:error` key.
  Struct slots `:value` and `:source` removed from `clime-param`.
  Dynamic vars `clime--building-values` and `clime--parse-values`
  removed; values map threaded as a local.  Conformer errors accumulate
  as `:error` entries before signaling a compound error.

## 0.5.0 — 2026-03-25

### Breaking changes

- **`:env` opt-in required**: options no longer auto-derive env var
  names from `:env-prefix` alone.  Add `:env` (or `:env t`) to each
  option that should participate in env var resolution.  Explicit
  `:env "SUFFIX"` still works as before.

### Features

- **`clime-invoke` structured return**: `clime-invoke` now returns
  `(:params PLIST :exit EXIT :output OUTPUT)` instead of just the
  params plist.  `:exit` is the handler exit code (integer or nil if
  the user quit), `:output` is the captured handler output string.
  Enables programmatic pipelines without inspecting buffer side effects.

- **Help env annotations**: CLI `--help` output now shows env var names
  on options that participate in env resolution.  Format: `[$VAR]` or
  `[$VAR=value]` when the variable is set in the environment.

- **Help `(required)` annotation**: options marked `:required` with no
  `:default` display a `(required)` suffix in help text.

- **`:conform` accepts 1-arg and 2-arg signatures**: option/arg
  conformers can use `(lambda (val) ...)` (value only) or
  `(lambda (val param) ...)` (value + param struct).  Arity is
  detected automatically.

- **Invoke: conformer inline validation**: the validation pipeline now
  runs param conformers in the invoke menu after every mutation,
  attributing errors to specific options inline.

- **Invoke: locked conformer groups**: when an inline group member is
  locked (e.g. via alias `:vals`), the entire conformer group is locked
  in the invoke menu.

- **Invoke: locked option metadata**: locked options display their full
  metadata (value, source) in the invoke menu instead of being hidden.

- **Invoke: visibility toggle**: `?` key cycles through three display
  modes — `normal` (default), `all` (reveals hidden items with
  annotation), `clean` (also hides deprecated).  Current mode shown in
  footer.

- **`make strip` command**: strips shebangs from tracked `.el` files
  before commits.  `CLIME_MAKE` env var available in batch mode.

### Fixes

- **`:env-prefix` with explicit `:env` suffix**: `:env-prefix` now
  correctly prepends to an explicit `:env` suffix instead of replacing
  it.

- **Alias `:vals`/`:defaults` in inline groups**: alias preset values
  now correctly resolve options inside mutex/zip groups.

- **Help: inline group children**: `--help` now displays options from
  inline group children (mutex/zip) in categorized sections.

- **Invoke quit/back keys**: reworked to `ESC`/`DEL`, freeing `q` for
  user app bindings.

## 0.4.0 — 2026-03-19

### Option groups

- **`clime-mutex`**: declare mutually exclusive options as a named group.
  At most one member can be set; the winner's name is injected as a
  derived key in ctx.  Supports `:default` for a fallback when no member
  is chosen.

  ```elisp
  (clime-mutex output-format
    (clime-opt json ("--json") :bool)
    (clime-opt csv  ("--csv")  :bool))

  ;; In handler:
  (clime-ctx-get ctx 'output-format) ;; => 'json, 'csv, or nil
  ```

- **`clime-zip`**: declare paired options that must be used the same
  number of times.  Values are zipped into a list of alists in ctx.

  ```elisp
  (clime-zip mappings
    (clime-opt from ("--from"))
    (clime-opt to   ("--to")))
  ;; --from a --to b --from c --to d
  ;; => (((from . "a") (to . "b")) ((from . "c") (to . "d")))
  ```

- **`clime-check-exclusive` and `clime-check-paired`**: factory functions
  for building node conformers programmatically.  Used internally by the
  DSL forms above; available for direct use in dynamic app construction.

### Conform system

- **Node-level `:conform`**: commands, groups, and apps now accept a
  `:conform` slot with signature `(params, node) → params`.  Runs
  during finalization in leaf→root order, after individual option/arg
  conformers.  Multiple conformers per node are supported (list of
  functions, threaded).

- **Option/arg `:conform` signature change**: updated from `(value)` to
  `(value, param)`.  Conformers can now inspect the param struct
  (name, flags, type, etc.).

- The cohort system (`:mutex`/`:zip`/`:cohort` slots on options,
  `clime-cohort` struct) has been replaced by the unified `:conform`
  mechanism.  `clime-mutex` and `clime-zip` DSL forms are the
  recommended migration path.

### Interactive menu (`clime-invoke`)

- **Rewritten as a lightweight `read-key` menu**, replacing the
  `transient.el` dependency.  The app tree IS the menu — no separate
  prefix/suffix definitions.  Groups become nested menus, options cycle
  with `-X` or set directly with `=X`, `RET` runs, `q` returns.  Zero
  external dependencies.

- **Inline validation**: the validation pipeline runs after every
  parameter mutation.  Per-param errors shown inline (`← error text`);
  conformer and `:requires` errors shown in the header.

- **`=` prefix for direct input**: `=` then an option key opens
  `completing-read` (choices) or `read-string` (counts) instead of
  cycling.

- **Prefix state feedback**: pressing `-` or `=` highlights option keys
  with the prefix (`-v`/`=v`) and dims non-option keys.

### Form composition

- **`clime-defcommand` and `clime-defgroup`**: define reusable command
  and group forms as `defvar` values, composable across files:

  ```elisp
  (clime-defcommand my-show
    :help "Show a resource"
    (clime-arg id :help "ID")
    (clime-handler (ctx) ...))

  (clime-app myapp
    :version "1.0"
    :children (list my-show)
    (clime-command add ...))
  ```

- **Keyword merge in containers**: `clime-app`, `clime-group`, and
  `clime-command` accept `:options`, `:args`, `:children`, and
  `:output-formats` keywords holding runtime list expressions.  Merged
  with inline DSL forms (keyword items first, then inline).

### DSL improvements

- **`clime-opt`** is now the canonical short form for options (matching
  `clime-arg`).  `clime-option` remains as a long-form alias.

- **Bare boolean keywords**: write `:bool :hidden` instead of
  `:bool t :hidden t`.  Works across all DSL forms.

- **`:examples` slot**: structured example invocations on commands,
  groups, and apps.  Rendered as a two-column "Examples:" section in
  `--help`.

- **`clime-param` base struct**: `clime-option` and `clime-arg` now
  share a common base with universal accessors (`clime-param-name`,
  `clime-param-help`, etc.).

- **Construction-time validation**: `clime-make-app` validates the
  entire tree at definition time — duplicate flags, option names, child
  names, and ancestor collisions.  Errors caught immediately, not at
  first parse.

### Changed

- **`clime-app` macro** now expands to `(progn (defvar NAME) (setq
  NAME ...))`.  Re-evaluating the form updates the variable in place.

- **`clime-reload`** self-reloads `clime.el` first and clears face specs
  before reloading optional modules, so `defface` changes take effect.

- **Auto-mutex on output formats removed**: use `clime-mutex` or node
  `:conform` with `clime-check-exclusive` for output format exclusivity.

### Fixed

- Resolved aliases now have correct parent refs, fixing ancestor option
  inheritance through aliases.
- Invoke menu exit code now reflects errors accumulated during handler
  execution.
- `clime-help-requested` in invoke menu no longer signals an unhandled
  error.

## 0.3.0 — 2026-03-13

### Changed

- **`:bool` replaces `:flag`**: the `:flag t` DSL shorthand is renamed to
  `:bool t` to avoid confusion with the `:flags` slot (list of flag
  strings).  `:flag t` still works but emits a deprecation warning.

- **Alias struct**: `clime-alias-for` now creates a `clime-alias` node
  (a `clime-node` subtype) instead of a `clime-command` with alias
  slots.  The DSL is unchanged; this is an internal refactor that
  enables future group aliasing.

- **Format-driven output**: replaced `clime-output-mode` symbol variable
  with `clime-out--active-format` (always a `clime-output-format` struct).
  Every output mode — including default text — is a format struct with
  `:encoder`, `:error-handler`, `:finalize`, and `:streaming` slots.  No if/else
  branching in output functions.  Handlers query the active format via `(clime-out-format)`.
  The `:json-mode` DSL keyword is unchanged (deprecated).

- **DSL forms are real macros**: `clime-option`, `clime-arg`,
  `clime-command`, `clime-group`, `clime-alias-for`, `clime-handler`
  are now `defmacro`/`cl-defmacro` forms.  Each produces its struct
  when evaluated standalone (REPL-friendly).  Standard `emacs-lisp-mode`
  keyword completion works inside any DSL form — no custom capf needed.

- **JSON output accumulation**: in JSON mode, `clime-out` calls are
  buffered and flushed as a single JSON array (2+ items) or bare object
  (1 item) after the handler returns.  Replaces the previous NDJSON
  behavior.  Use `clime-out-emit` for explicit NDJSON when needed.
  Handler return-value wrapping in `{success, data}` envelope is
  preserved when no `clime-out` calls are made.  Errors accumulate
  via `clime-out-error` and are passed to finalize — errors take
  priority over items and retval in the default envelope.

- **`clime-output-format` DSL form**: declares output modes as first-class
  CLI options.  Derives from `clime-option`, inheriting `:mutex`,
  `:hidden`, `:category`, etc.  Supports `:finalize` for custom envelope
  shapes and `:streaming` to bypass the accumulator.  `:json-mode t` is
  kept as deprecated sugar.  Multiple output formats are auto-mutexed.

### Added

- `clime-alias-for` DSL form: declare a command as an alias for a nested
  command path.  Copies args, options, and handler from the target at init
  time, eliminating boilerplate when exposing nested commands at a higher
  level.  Supports `:help`, `:aliases`, `:hidden`, `:category` overrides,
  transitive resolution, and circular chain detection.
  - `:defaults '((name . value) ...)` — override default values on
    copied options.  Visible in help, overridable by CLI.
  - `:vals '((name . value) ...)` — lock option values.  The option
    is removed from CLI and help; the value is injected into params
    unconditionally.
- `clime-invoke`: interactive transient.el UI for clime apps.  Auto-generates
  menus from `clime-app` definitions — groups become nested prefixes, options
  become infixes, leaf commands get a "Run" action.  Requires the `transient`
  package (ships with Emacs 29+).
- `clime-reload`: force-reload all clime modules in dependency order during
  development, including optional modules (`clime-invoke`, `clime-make`) if
  already loaded.  Invalidates the invoke prefix cache.
- `clime-run-batch` is now a no-op in interactive Emacs (previously called
  `kill-emacs`), with a warning message.
- Terminal-width-aware help text wrapping: help descriptions, epilog text,
  and table right-columns now wrap at the terminal width.  Auto-detects
  from `COLUMNS` env var (fallback 80, minimum 40).  Override with
  `clime-help-width` variable.
- DSL indent rules: `lisp-indent-function` properties on `clime-option` (2),
  `clime-arg`, `clime-command`, `clime-group`, `clime-handler` (1) for
  correct Emacs indentation.
- `clime-main-script-p` guard on `clime-make.el` entrypoint: `(require
  'clime-make)` from interactive Emacs no longer triggers the CLI.
- `:deprecated` slot on options, args, commands, and groups.  Accepts a
  string (migration hint) or `t` (generic).  Shows "(deprecated)" in help
  output and emits a warning to stderr at runtime.  Combine with `:hidden t`
  for silent deprecation.
- `:negatable` slot on options: auto-generate `--no-X` variants for
  boolean flags.  `--no-X` explicitly sets the param to nil, allowing
  handlers to distinguish unset/true/false (ternary state).  Implies
  boolean (no `:bool t` needed).  Help renders `--flag / --no-flag`.
- `clime-param` accessor: `(clime-param ctx 'name default)` returns
  the param value if set (even nil), or DEFAULT if absent.  Complements
  `clime-ctx-get` for ternary-aware param access.
- `:mutex` slot on options: declare mutually exclusive option groups.
  Options sharing the same `:mutex` symbol (e.g. `:mutex 'output-format`)
  are validated to be mutually exclusive — at most one may be set per
  invocation.  Defaults on mutex siblings are suppressed when any member
  is explicitly set (CLI or env var).
- `:requires` slot on options: declare directional dependencies between
  options.  `:requires '(reason)` on `--skip` means `--skip` can only
  be used when `--reason` is also provided.  Validates after env vars
  are applied; defaults do not satisfy the requirement.  Supports
  one-way deps, multi-deps, and cross-level (ancestor) options.
- `:zip` slot on options: declare paired option groups where `:multiple`
  options must be used the same number of times.  Values are zipped into
  a list of alists available in ctx under the group name.  Implies
  `:multiple t`.
- `clime-output-format`: DSL form for declaring output formats.  Derives
  from `clime-option` — supports `:finalize` (custom envelope function),
  `:streaming` (bypass accumulator for NDJSON), and all standard option
  keywords (`:mutex`, `:hidden`, `:category`, etc.).
- `clime-out-emit`: emit NDJSON immediately, bypassing the output
  accumulator.  For streaming use cases where per-call emission is
  desired.  (Renamed from `clime-output-stream`.)
- `clime-out-format`: return the active output format's name symbol
  (e.g. `text`, `json`).  (Renamed from `clime-output-name`.)
- `clime-out--finalize-default`: named default finalize function.
  Signature: `(items retval errors)`.  Priority: errors → `{error}`
  envelope, items → array/bare, retval → `{success, data}`, nil → nil.
- `clime-out-error`: report errors via the active format.  Streaming
  formats dispatch immediately; buffered formats accumulate errors for
  finalize.  (Renamed from `clime-output-error`.)
- `examples/cloq.el`: demo app wrapping org-ql into a CLI tool,
  showcasing output formats, aliases, and JSON mode.

### Removed

- `clime-format-error` defvar and `clime--format-error-default`: error
  formatting is now handled by the `:error-handler` slot on each
  `clime-output-format` struct.
- `clime-output-mode-json-p` (deprecated): use
  `(eq (clime-out-format) 'json)` instead.

### Fixed

- Inline group names are now excluded from user-facing error paths and
  "Try X --help" hints (internal parse path still includes them).

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
