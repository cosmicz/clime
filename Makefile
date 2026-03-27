EMACS ?= emacs

BATCH = $(EMACS) --batch -Q -L . -L ./tests
CLIME_MAKE = $(BATCH) -l clime-make-main.el --

.PHONY: all compile lint test tests dist bin init strip readme clean clean-elc help

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

SELECT ?= ^clime-test-
SELECTOR ?= $(SELECT)

test: clean-elc
	@$(BATCH) -l ./tests/clime-tests-runner.el \
		--eval '(clime-run-tests-batch "$(SELECTOR)")' \
		< /dev/null

tests: test

DIST_DIR ?= dist
DIST_SRCS = clime-settings.el clime-core.el clime-parse.el \
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

# Auto-discover straight.el repos for cloq dependencies
STRAIGHT_DIR ?= $(HOME)/.emacs.d/.local/straight/repos
CLOQ_DEPS_LIST := org-ql s.el dash.el ts.el
CLOQ_LP := $(foreach d,$(CLOQ_DEPS_LIST),-L $(STRAIGHT_DIR)/$(d))

bin/clime: $(DIST_SRCS) clime-make-main.el
	@mkdir -p $(BIN_DIR)
	@$(MAKE) --no-print-directory dist
	@cp $(DIST_DIR)/clime.el $@

bin/cloq: examples/cloq.el
	@mkdir -p $(BIN_DIR)
	@$(CLIME_MAKE) init $< -o $@ -L .. $(CLOQ_LP) \
		--env CLIME_MAIN_APP=cloq

bin/greeter: examples/greeter.el
	@mkdir -p $(BIN_DIR)
	@$(CLIME_MAKE) init --client $< -o $@ -R .. --standalone

bin: bin/clime bin/cloq bin/greeter

CLOQ_DEPS ?=

init:
	@$(CLIME_MAKE) quickstart clime-make.el --self-dir --standalone
	@$(CLIME_MAKE) init examples/pkm.el -R .. --standalone
	@$(CLIME_MAKE) init examples/cloq.el -R .. --standalone \
		--env CLIME_MAIN_APP=cloq $(CLOQ_DEPS)

strip:
	@for f in clime-make.el examples/*.el; do \
		if head -1 "$$f" | grep -q '^#!'; then \
			$(CLIME_MAKE) strip "$$f"; \
		fi; \
	done

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
	@echo "  all      - Default target. Same as 'compile'"
	@echo "  compile  - Byte-compile all Elisp files"
	@echo "  lint     - Byte-compile with warnings + checkdoc"
	@echo "  test     - Run tests (SELECT= to filter)"
	@echo "  tests    - Alias for 'test'"
	@echo "  dist     - Build single-file dist/clime.el bundle"
	@echo "  bin      - Build local executables in bin/ (gitignored)"
	@echo "             STRAIGHT_DIR= to override straight.el repos path"
	@echo "  init     - Add shebangs to clime-make.el and examples"
	@echo "             CLOQ_DEPS='-L /path/to/org-ql ...' for cloq deps"
	@echo "  strip    - Remove shebangs from clime-make.el and examples"
	@echo "  readme   - Export README.org to README.md"
	@echo "  clean    - Remove .elc files and build artifacts"
	@echo "  help     - Show this help message"
