EMACS ?= emacs

BATCH = $(EMACS) --batch -Q -L . -L ./tests

.PHONY: all compile lint test tests dist readme clean help

all: compile

compile:
	@echo "Compiling clime Elisp files..."
	@$(BATCH) -f batch-byte-compile *.el

lint:
	@echo "Running lint checks..."
	@$(BATCH) --eval '(require (quote bytecomp))' --eval '(setq byte-compile-error-on-warn t byte-compile-warnings (quote (not docstrings-wide)))' -f batch-byte-compile *.el
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

test:
	@$(BATCH) -l ./tests/clime-tests-runner.el \
		--eval '(clime-run-tests-batch "$(SELECTOR)")'

tests: test

DIST_DIR ?= dist
DIST_SRCS = clime-settings.el clime-core.el clime-parse.el \
	clime-dsl.el clime-help.el clime-output.el clime-run.el clime.el \
	clime-app.el

dist:
	@./clime-app.el bundle -o $(DIST_DIR)/clime.el \
		--provide clime --main clime \
		--description "Declarative CLI framework for Emacs Lisp" \
		$(DIST_SRCS)
	@./clime-app.el init --standalone --env CLIME_MAIN_APP=clime \
		$(DIST_DIR)/clime.el

readme:
	@echo "Exporting README.org → README.md..."
	@$(EMACS) --batch -Q README.org \
		--eval '(require (quote ox-md))' \
		--eval '(org-md-export-to-markdown)'
	@echo "Done."

clean:
	@echo "Cleaning up compilation artifacts..."
	@rm -f *.elc tests/*.elc
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
	@echo "  readme   - Export README.org to README.md"
	@echo "  clean    - Remove .elc files and build artifacts"
	@echo "  help     - Show this help message"
