EMACS ?= emacs

BATCH = $(EMACS) --batch -Q -L . -L ./tests
CLIME_MAKE = $(BATCH) -l clime-make-main.el --

.PHONY: all compile lint test test-all tests dist bins readme clean clean-elc help

all: compile

SRCS := $(filter-out clime-make-main.el,$(wildcard *.el))

compile:
	@echo "Compiling clime Elisp files..."
	@$(BATCH) -f batch-byte-compile $(SRCS)

lint:
	@echo "Running lint checks..."
	@$(BATCH) --eval '(require (quote bytecomp))' --eval '(setq byte-compile-error-on-warn t byte-compile-warnings (quote (not docstrings-wide)) byte-compile-docstring-max-column 10000)' -f batch-byte-compile $(SRCS)
	@echo "Byte-compile clean."
	@$(BATCH) --eval '\
	  (let ((files (directory-files "." t "^clime.*\\.el$$")) \
	        (ok t)) \
	    (dolist (f files) \
	      (with-temp-buffer \
	        (insert-file-contents f) \
	        (emacs-lisp-mode) \
	        (condition-case err \
	            (checkdoc-current-buffer t) \
	          (error (setq ok nil) \
	                 (message "checkdoc: %s: %s" f (error-message-string err)))))) \
	    (unless ok (kill-emacs 1)))' \
		2>&1 || true
	@echo "Lint complete."

SEL ?= ^clime-test-
SELECT ?= $(SEL)
SELECTOR ?= $(SELECT)
# VERBOSE=1 to show message output from tests (not swallowed by ERT)
VERBOSE ?=

tests: test
test: clean-elc
	@CLIME_TEST_VERBOSE=$(VERBOSE) $(BATCH) -l ./tests/clime-tests-runner.el \
		--eval '(clime-run-tests-batch "$(SELECTOR)")' \
		< /dev/null

test-all: bin/clime-make bin/greeter bin/pkm test

DIST_DIR ?= dist
DIST_SRCS = clime-settings.el clime-core.el clime-param-type.el clime-parse.el \
	clime-dsl.el clime-help.el clime-output.el clime-run.el clime.el \
	clime-make.el

dist:
	@$(CLIME_MAKE) bundle -o $(DIST_DIR)/clime.el \
		--provide clime --main clime-make-main.el \
		--description "Declarative CLI framework for Emacs Lisp" \
		$(DIST_SRCS)
	@$(CLIME_MAKE) init --standalone --env CLIME_MAIN_APP=clime \
		$(DIST_DIR)/clime.el

# ── Local executables (gitignored) ─────────────────────────────────
BIN_DIR := bin

$(BIN_DIR):
	@mkdir -p $@

# Auto-discover straight.el repos for cloq dependencies
STRAIGHT_DIR ?= $(HOME)/.emacs.d/.local/straight/repos
CLOQ_DEPS_LIST := org-ql s.el dash.el ts.el
CLOQ_LP := $(foreach d,$(CLOQ_DEPS_LIST),-L $(STRAIGHT_DIR)/$(d))

# Bootstrap: build clime-make using batch loader
bin/clime-make: clime-make.el | $(BIN_DIR)
	@$(CLIME_MAKE) quickstart $< -o $@ -L . --standalone --env CLIME_MAIN_APP=clime-make

bin/clime: $(DIST_SRCS) clime-make-main.el | $(BIN_DIR)
	@$(MAKE) --no-print-directory dist
	@cp $(DIST_DIR)/clime.el $@

# Examples use built clime-make as a self-check
bin/greeter: examples/greeter.el bin/clime-make | $(BIN_DIR)
	@./bin/clime-make quickstart $< -o $@ -R .. --standalone

bin/cloq: examples/cloq.el bin/clime-make | $(BIN_DIR)
	@./bin/clime-make quickstart $< -o $@ -R .. --standalone \
		--env CLIME_MAIN_APP=cloq $(CLOQ_LP)

bin/pkm: examples/pkm.el bin/clime-make | $(BIN_DIR)
	@./bin/clime-make quickstart $< -o $@ -R .. --standalone

bins: bin/clime bin/clime-make bin/cloq bin/greeter bin/pkm

readme:
	@echo "Exporting README.org → README.md..."
	@$(EMACS) --batch -Q README.org \
		--eval '(require (quote ox-md))' \
		--eval '(org-md-export-to-markdown)'
	@echo "Done."

clean-elc:
	@rm -f *.elc tests/*.elc

clean: clean-elc
	@echo "Cleaning up all build artifacts..."
	@rm -rf .packages $(DIST_DIR) $(BIN_DIR)
	@echo "Done."

help:
	@echo "clime Makefile targets:"
	@echo "  all       - Default target. Same as 'compile'"
	@echo "  compile   - Byte-compile all Elisp files"
	@echo "  lint      - Byte-compile with warnings + checkdoc"
	@echo "  test      - Run tests (SEL= to filter, VERBOSE=1 for messages)"
	@echo "  test-all  - Build executables then run full test suite"
	@echo "  tests     - Alias for 'test'"
	@echo "  dist      - Build single-file dist/clime.el bundle"
	@echo "  bins      - Build all local executables in bin/ (gitignored)"
	@echo "              STRAIGHT_DIR= to override straight.el repos path"
	@echo "  bin/clime-make - Build clime-make standalone binary"
	@echo "  bin/clime      - Build bundled clime binary"
	@echo "  bin/greeter    - Build greeter example (emacsclient wrapper)"
	@echo "  bin/cloq       - Build cloq example"
	@echo "  bin/pkm        - Build pkm example"
	@echo "  readme    - Export README.org to README.md"
	@echo "  clean     - Remove .elc files and build artifacts"
	@echo "  clean-elc - Remove only .elc files"
	@echo "  help      - Show this help message"
