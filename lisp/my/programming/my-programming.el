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

;; TODO describe this file.

;;; Code:

(use-my lib)

(use-package hl-line
  :after prog-mode
  :hook
  (prog-mode-hook . hl-line-mode))

(use-package display-line-numbers
  :after prog-mode
  :hook
  (prog-mode-hook . display-line-numbers-mode))

(use-package electric
  :after prog-mode
  :config
  (push ?\^? electric-indent-chars)
  :hook
  (prog-mode-hook . electric-indent-local-mode))

(use-package smartparens-mode
  :ensure smartparens
  :pin nongnu
  :after prog-mode
  :config
  (require 'smartparens-config)
  :hook
  (prog-mode-hook . smartparens-mode))

(use-package paren
  :init
  (setq-default show-paren-mode nil)
  :hook
  (prog-mode-hook . show-paren-local-mode))

(use-package rainbow-delimiters
  :ensure rainbow-delimiters
  :pin nongnu
  :hook
  (prog-mode-hook . rainbow-delimiters-mode))

(use-package hideshow
  :after prog-mode
  :hook
  (prog-mode-hook . hs-minor-mode))

(use-package company
  :ensure company
  :pin gnu
  :config
  (-snocq company-backends '(company-capf :with company-yasnippet))
  :hook
  (prog-mode-hook . company-mode))

(use-package yasnippet
  :ensure yasnippet
  :pin gnu
  :hook
  (prog-mode-hook . yas-minor-mode))

(use-package eglot
  :ensure eglot
  :pin gnu
  :commands
  (eglot eglot-ensure))

(use-package flymake
  :ensure flymake
  :pin gnu
  :config
  (keymap-set flymake-mode-map "C-c C-!" 'consult-flymake))

(use-package flymake-popon
  :ensure flymake-popon
  :pin nongnu
  :hook
  (flymake-mode-hook . flymake-popon-mode))

(use-my programming-emacs-lisp)

(use-my programming-lisp)

(use-my programming-nix)

(use-my programming-yaml)

(provide 'my-programming)
;;; my-programming.el ends here
