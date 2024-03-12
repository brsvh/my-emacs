;;; my-editor.el --- Editing enhancements of My Emacs -*- lexical-binding: t -*-

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

(use-package mwim
  :vc (:url "https://github.com/alezost/mwim.el.git")
  :keymap-set
  ("<remap> <move-beginning-of-line>" . mwim-beginning-of-code-or-line)
  ("<remap> <move-end-of-line>" . mwim-end-of-code-or-line))

(use-package delsel
  :hook
  (on-first-buffer-hook . delete-selection-mode))

(use-package simple
  :config
  (setq kill-do-not-save-duplicates t
        save-interprogram-paste-before-kill t)

  :hook
  (on-first-buffer-hook . column-number-mode)
  (on-first-buffer-hook . line-number-mode)
  (on-first-buffer-hook . size-indication-mode))

(use-package consult
  :keymap-set
  ("<remap> <yank>" . consult-yank-from-kill-ring)
  ("<remap> <yank-pop>" . consult-yank-pop))

(use-package files
  :config
  (setq auto-save-default t)

  (setq auto-save-no-message t)

  (setq auto-save-include-big-deletions t)

  (setq auto-save-file-name-transforms
        (append `((".*"
                   ,(my-data-path* "auto-save/")
                   t))
                auto-save-file-name-transforms))

  (setq auto-save-list-file-prefix (my-data-path* "auto-save/lists/"))

  (setq make-backup-files t
        version-control t)

  (setq backup-by-copying t)

  (setq delete-old-versions t
        kept-old-versions 5
        kept-new-versions 5)

  (setq backup-directory-alist
        (append `(("." . ,(my-data-path* "backup/")))
                backup-directory-alist)))

(use-package saveplace
  :config
  (setq save-place-file (my-state-path "place.el"))
  :hook
  (on-first-file-hook . save-place-mode))

(use-package recentf
  :config
  (define-advice recentf-load-list
      (:around (fn &rest args) silent-message)
    "Silencing load message."
    (cl-letf (((symbol-function #'message) #'ignore))
      (apply fn args)))

  (define-advice recentf-cleanup
      (:around (fn &rest args) silent-message)
    "Silencing clean up message."
    (cl-letf (((symbol-function #'message) #'ignore))
      (apply fn args)))

  (setq recentf-save-file (my-state-path "recent.el"))
  :hook
  (on-first-file-hook . recentf-mode))

(use-package consult
  :keymap-set
  (:ctl-c-f-map
   ("r" . consult-recent-file)))

(use-package autorevert
  :config
  (setq auto-revert-verbose nil)
  :hook
  (on-first-buffer-hook . global-auto-revert-mode))

(use-package editorconfig
  :ensure editorconfig
  :hook
  (on-first-file-hook . editorconfig-mode))

(use-package company
  :ensure company
  :pin gnu
  :config
  (keymap-set company-active-map "M-/" 'company-complete)

  (setq company-tooltip-align-annotations t))

(use-package reformatter
  :ensure reformatter
  :pin nongnu)

(use-package yasnippet
  :ensure yasnippet
  :pin gnu
  :config
  (setq yas-snippet-dirs `(,(my-config-path "etc/" "snippets/")))

  (define-advice yas-load-directory
      (:around (orig-func &rest args) silent-message)
    "Inhibit the messages of `yasnippet' loading."
    (cl-letf (((symbol-function #'message) #'ignore))
      (apply orig-func args))))

(use-package yasnippet-snippets
  :ensure yasnippet-snippets
  :pin nongnu
  :after yasnippet)

(provide 'my-editor)
;;; my-editor.el ends here
