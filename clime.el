;;; clime.el --- TODO: describe your package  -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author:
;; Keywords:
;; Package-Requires: ((emacs "29.1"))
;; Version: 0.1.0

;; This file is not part of GNU Emacs.

;; Permission is hereby granted, free of charge, to any person obtaining
;; a copy of this software and associated documentation files (the
;; "Software"), to deal in the Software without restriction, including
;; without limitation the rights to use, copy, modify, merge, publish,
;; distribute, sublicense, and/or sell copies of the Software, and to
;; permit persons to whom the Software is furnished to do so, subject to
;; the following conditions:
;;
;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.

;;; Commentary:

;; Entry point for clime.
;;
;; This file provides autoloads and top-level require for the package.

;;; Code:

(require 'clime-settings)
(require 'clime-core)
(require 'clime-parse)
(require 'clime-dsl)

(provide 'clime)
;;; clime.el ends here
