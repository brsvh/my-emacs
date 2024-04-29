;;; my-programming.el --- Porgramming support of My Emacs -*- lexical-binding: t -*-

;; Copyright (C) 2022-2024 Burgess Chang

;; Author: Burgess Chang <bsc@brsvh.org>
;; Keywords: local
;; Package-Requires: ((emacs "29.1"))
;; URL: https://github.com/brsvh/my-emacs
;; Version: 0.1.50

;; This file is part of my-emacs.

;; my-emacs is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the
;; Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.

;; my-emacs is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with my-emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This file has enhanced my general programming experience, and
;; individually enable support for different programming languages.

;; Supported programming languages:
;;   - Emacs Lisp

;;; Code:

(require 'my-core)
(require 'my-programming-emacs-lisp)
(require 'my-programming-nix)

(cl-eval-when (compile)
  (require 'apheleia)
  (require 'rainbow-delimiters)
  (require 'smartparens))



;;;
;; Appearance:

(setup rainbow-delimiters
  (:autoload rainbow-delimiters-mode))

(setup prog-mode
  (:hook
   #'display-line-numbers-mode ;; Show line numbers of buffer.
   #'hl-line-mode              ;; Hightlight current line of buffer.
   #'rainbow-delimiters-mode   ;; Colorful brackets highlighting.
   ))



;;;
;; Editing:

(setup electric
  (:when-loaded
    (push ?\^? electric-indent-chars)))

(setup smartparens
  (:autoload smartparens-mode)
  (:also-load smartparens-config))

(setup prog-mode
  (:hook
   #'electric-indent-local-mode ;; Auto reindentation.
   #'smartparens-mode           ;; Auto insert paired paren.
   ))



;;;
;; Format:

(setup apheleia
  (:first-buffer apheleia-global-mode))



(provide 'my-programming)
;;; my-programming.el ends here
