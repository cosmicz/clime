EMACS ?= emacs

BATCH = $(EMACS) --batch -Q -L . -L ./tests

.PHONY: all compile lint test tests clean help

all: compile

compile:
	@echo "Compiling clime Elisp files..."
	@$(BATCH) -f batch-byte-compile *.el

lint:
	@echo "Running lint checks..."
	@$(BATCH) --eval '(setq byte-compile-error-on-warn t)' \
		-f batch-byte-compile *.el
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

clean:
	@echo "Cleaning up compilation artifacts..."
	@rm -f *.elc tests/*.elc
	@rm -rf .packages
	@echo "Done."

help:
	@echo "clime Makefile targets:"
	@echo "  all      - Default target. Same as 'compile'"
	@echo "  compile  - Byte-compile all Elisp files"
	@echo "  lint     - Byte-compile with warnings + checkdoc"
	@echo "  test     - Run tests (SELECT= to filter)"
	@echo "  tests    - Alias for 'test'"
	@echo "  clean    - Remove .elc files and build artifacts"
	@echo "  help     - Show this help message"
