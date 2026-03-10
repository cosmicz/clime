;;; clime-make-main.el --- Entrypoint for clime-make  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Cosmin Octavian

;; Author: Cosmin Octavian <cosmicz@protonmail.com>

;; SPDX-License-Identifier: MIT
;; This file is not part of GNU Emacs.

;;; Commentary:

;; Standalone entrypoint for the clime CLI tool.  Used by the bundler
;; as the --main file for dist builds.

;;; Code:

(require 'clime-make)
(clime-run-batch clime-make)

;;; clime-make-main.el ends here
