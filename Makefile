EMACS ?= emacs

BATCH = $(EMACS) --batch -Q -L . -L ./tests

.PHONY: all compile lint test tests dist init readme clean clean-elc help

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
	@./clime-make.el bundle -o $(DIST_DIR)/clime.el \
		--provide clime --main clime-make-main.el \
		--description "Declarative CLI framework for Emacs Lisp" \
		$(DIST_SRCS)
	@./clime-make.el init --standalone --env CLIME_MAIN_APP=clime \
		$(DIST_DIR)/clime.el

init:
	@./clime-make.el init clime-make.el --self-dir --standalone
	@./clime-make.el init examples/pkm.el -R .. --standalone

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
	@rm -rf .packages $(DIST_DIR)
	@echo "Done."

help:
	@echo "clime Makefile targets:"
	@echo "  all      - Default target. Same as 'compile'"
	@echo "  compile  - Byte-compile all Elisp files"
	@echo "  lint     - Byte-compile with warnings + checkdoc"
	@echo "  test     - Run tests (SELECT= to filter)"
	@echo "  tests    - Alias for 'test'"
	@echo "  dist     - Build single-file dist/clime.el bundle"
	@echo "  init     - Update shebangs on executable scripts"
	@echo "  readme   - Export README.org to README.md"
	@echo "  clean    - Remove .elc files and build artifacts"
	@echo "  help     - Show this help message"
