;;; my-project.el --- Project management of My Emacs -*- lexical-binding: t -*-

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

(use-package rg
  :ensure rg
  :demand t)

(use-package project
  :config
  (setq project-list-file (my-state-path "projects.el")))

(use-package vc-dir
  :preface
  (-snocq popper-reference-buffers
          'vc-dir-mode))

(use-package vc-git
  :preface
  (-snocq popper-reference-buffers "^\\*vc-git \: .*\\*$"))

(use-package projectile
  :ensure projectile
  :config
  (setq ctl-c-p-map (copy-keymap projectile-command-map))
  (keymap-set ctl-c-map "p" ctl-c-p-map)

  (setq projectile-cache-file (my-state-path* "projectile/" "cache.el")
        projectile-known-projects-file (my-state-path* "projectile/"
                                                       "projects.el"))
  :hook
  (on-first-file-hook . projectile-mode))

(use-package diff-hl
  :ensure diff-hl
  :functions
  (diff-hl-magit-pre-refresh
   diff-hl-magit-post-refresh)
  :hook
  (find-file-hook . diff-hl-mode))

(use-package diff-hl-flydiff
  :ensure diff-hl
  :after diff-hl
  :hook
  (diff-hl-mode-hook . diff-hl-flydiff-mode))

(use-package diff-hl-margin
  :ensure diff-hl
  :after diff-hl
  :init
  (defun my--inhibit-diff-hl-margin-mode ()
    "Enable `'diff-hl-margin-mode' on in non-graphic frame."
    (if diff-hl-mode
        (if (display-graphic-p)
            (diff-hl-margin-mode -1)
          (diff-hl-margin-mode +1))
      (diff-hl-margin-mode -1)))

  (add-hook 'diff-hl-mode-hook #'my--inhibit-diff-hl-margin-mode))



;; VCS (Git):

(use-package magit
  :ensure magit
  :init
  (setq-default magit-define-global-key-bindings nil)
  :keymap-set
  (:ctl-c-v-g-map
   ("d" . magit-dispatch)
   ("s" . magit-status))
  :hook
  (magit-pre-refresh-hook . diff-hl-magit-pre-refresh)
  (magit-post-refresh-hook . diff-hl-magit-post-refresh))

(use-package git-modes
  :ensure git-modes)

(use-package gitattributes-mode
  :ensure git-modes)

(use-package gitconfig-mode
  :ensure git-modes)

(use-package gitignore-mode
  :ensure git-modes)



;; Nix project:

(use-package nix3
  :vc (:url "https://github.com/emacs-twist/nix3.el")
  :keymap-set
  (:ctl-c-n-map
   ("b" . nix3-build)
   ("d" . nix3-transient)
   ("e" . nix3-flake-edit)
   ("i" . nix3-flake-init)
   ("n" . nix3-flake-new)
   ("r" . nix3-run)
   ("s" . nix3-flake-show)))

(provide 'my-project)
;;; my-project.el ends here
