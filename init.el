;;; init.el --- Init File -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Burgess Chang

;; Author: Burgess Chang <bsc@brsvh.org>
;; Keywords: internal
;; Version: 0.50.0

;; This file is part of emacs.d.

;; emacs.d is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; emacs.d is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with emacs.d. If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This file is first loaded file after Emacs is started, we set up
;; the essential dependencies and preset configuration for our Emacs
;; integrated computing environment.

;;; Code:

(require 'early-init (concat user-emacs-directory "early-init.el"))
(require 'use-package)

(use-package use-package
  :init
  ;; Make `use-package' always use proper hook name.
  (setq use-package-hook-name-suffix nil)

  ;; Enable `imenu' support of `use-package'.
  (setq use-package-enable-imenu-support t)

  ;; Always defer load.
  (setq use-package-always-defer t))

(use-package exec-path-from-shell
  :ensure t
  :pin melpa
  :autoload exec-path-from-shell-initialize
  :init
  ;; Ensure environment variables inside Emacs look the same as in the
  ;; user's shell.
  (when (memq window-system '(mac ns x pgtk))
    (exec-path-from-shell-initialize)))

(use-package emacs
  :no-require t
  :init
  ;; Enable recursive minibuffers.
  (setq enable-recursive-minibuffers t)

  ;; Resize windows pixelwise.
  (setq window-resize-pixelwise t)

  ;; Resize frame pixelwise.
  (setq frame-resize-pixelwise t)

  ;; Automatically adjust ‘window-vscroll’ to view tall lines.
  (setq auto-window-vscroll nil)

  ;; Accelerate scrolling operations
  (setq fast-but-imprecise-scrolling t)

  ;; Kell current position after screen scroll.
  (setq scroll-preserve-screen-position t)

  ;; When at the bottom or top of the buffer, move only one line.
  (setq scroll-margin 0 scroll-conservatively 101)

  ;; Horizontally scrolling threshold.
  (setq hscroll-margin 2 hscroll-step 1)

   ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))

  ;; Disable case-sensitivity for completion.
  (setq completion-ignore-case t)

  ;; Inhibit draw the underline at the same place as the descent line.
  (setq x-underline-at-descent-line t))

(use-package gcmh
  :ensure t
  :pin gnu
  :hook (emacs-startup-hook . gcmh-mode))

(use-package modus-themes
  :ensure t
  :pin gnu
  :autoload (modus-themes-load-themes
             modus-themes-load-operandi
             modus-themes-load-vivendi)
  :init
  (setq modus-themes-bold-constructs t
        modus-themes-italic-constructs t
        modus-themes-subtle-line-numbers t
        modus-themes-inhibit-reload t
        modus-themes-fringes 'nil
        modus-themes-mode-line '(borderless moody)
        modus-themes-markup '(background italic)
        modus-themes-links '(italic neutral-underline)
        modus-themes-prompts '(intense bold)
        modus-themes-headings
        '((0 . (rainbow 1.2))
          (t . (rainbow))))
  (modus-themes-load-themes)
  :config
  (modus-themes-load-operandi)
  ;; Set more subtle line numbers.
  (when (member 'modus-operandi custom-enabled-themes)
    (set-face-attribute
     'line-number-current-line nil
     :background (face-attribute 'modus-themes-hl-line :background))))

;; Displaying elements of the mode line as tabs and ribbons.
(use-package moody
  :ensure t
  :pin melpa
  :autoload (moody-replace-mode-line-buffer-identification
             moody-replace-vc-mode
             moody-replace-eldoc-minibuffer-message-function)
  :init
  (moody-replace-mode-line-buffer-identification)
  (moody-replace-vc-mode)
  (moody-replace-eldoc-minibuffer-message-function))

;; Displaying a menu that lists enabled minor-modes.
(use-package minions
  :ensure t
  :pin melpa
  :defines minions-prominent-modes
  :init
  ;; :)
  (setq minions-mode-line-lighter ":)"
        minions-mode-line-delimiters '("" . ""))
  :hook (emacs-startup-hook . minions-mode))

(use-package startup
  :no-require t
  :init
  ;; Inhibit *GNU Emacs* buffer, message in echo area and *scratch*
  ;; buffer.
  (setq inhibit-startup-screen t
        inhibit-startup-echo-area-message t
        initial-scratch-message nil
        initial-major-mode 'fundamental-mode)

  ;; Redirect auto save files list.
  (setq auto-save-list-file-prefix
        (concat (expand-file-name "sessions/" my-data-directory)
                "saves-")))

(use-package server
  :autoload server-running-p
  :init
  (setq server-auth-dir
        (expand-file-name "server/" my-data-directory))
  :hook
  ;; Start server at startup.
  (emacs-startup-hook
   . (lambda ()
       (eval-when-compile (require 'server))
       (unless (server-running-p) (server-start)))))

(use-package auth-sources
  :init
  (setq auth-sources
        (list (expand-file-name "authinfo" my-data-directory)
              (expand-file-name "authinfo.gpg" my-data-directory))))

(use-package minibuffer
  :ensure orderless
  :init
  ;; TAB cycle if there are only a few candidates.
  (setq completion-cycle-threshold 5)

  ;; Use orderless style get more flexible completions.
  (setq completion-styles '(substring orderless basic)
        completion-category-overrides '((file (styles
                                               basic
                                               partial-completion))))

  ;; Disable case-sensitivity for file and buffer.
  (setq read-file-name-completion-ignore-case t
        read-buffer-completion-ignore-case t))

(use-package crm
  :preface
  ;; Add prompt indicator to `completing-read-multiple'.  We display
  ;; [CRM<separator>], e.g., [CRM,] if the separator is a comma.
  (defun my-crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  :init
  (advice-add #'completing-read-multiple
              :filter-args #'my-crm-indicator))

(use-package vertico
  :ensure t
  :pin gnu
  :commands vertico-mode
  :init
  ;; Grow and shrink the Vertico minibuffer
  (setq vertico-resize t)

  ;; enable cycling for `vertico-next' and `vertico-previous'.
  (setq vertico-cycle t)
  :config
  (keymap-set vertico-map "?" 'minibuffer-completion-help)
  (keymap-set vertico-map "M-RET" 'minibuffer-force-complete-and-exit)
  (keymap-set vertico-map "M-TAB" 'minibuffer-complete)
  :hook
  (emacs-startup-hook . vertico-mode))

(use-package vertico-directory
  :after vertico
  :autoload (vertico-directory-enter
             vertico-directory-delete-char
             vertico-directory-delete-word)
  :init
  ;; More convenient directory navigation commands
  (keymap-set vertico-map "RET" 'vertico-directory-enter)
  (keymap-set vertico-map "<return>" 'vertico-directory-enter)
  (keymap-set vertico-map "DEL" 'vertico-directory-delete-char)
  (keymap-set vertico-map "<backspace>" 'vertico-directory-delete-char)
  (keymap-set vertico-map "M-DEL" 'vertico-directory-delete-word)
  (keymap-set vertico-map "M-<backspace>" 'vertico-directory-delete-word)
  :hook
  ;; Tidy shadowed file names.
  (rfn-eshadow-update-overlay . vertico-directory-tidy))

(use-package vertico-mouse
  :after vertico
  :hook (vertico-mode-hook . vertico-mouse-mode))

(use-package marginalia
  :ensure t
  :pin gnu
  :config
  ; Make marginalia align to right.
  (setq marginalia-align 'right)
  :hook (emacs-startup-hook . marginalia-mode))

(use-package consult
  :ensure t
  :pin gnu
  :commands (consult-flymake consult-xref)
  :init
  (keymap-substitute global-map 'switch-to-buffer 'consult-buffer)
  (keymap-substitute global-map 'goto-line 'consult-goto-line)
  (keymap-substitute global-map 'imenu 'consult-imenu)
  (keymap-set global-map "M-g m" 'consult-mark)
  (keymap-set global-map "M-g o" 'consult-outline)
  (keymap-set global-map "M-g M-i" 'consult-imenu-multi)
  (keymap-set global-map "M-g M-m" 'consult-global-mark)
  (keymap-set global-map "M-s f" 'consult-find)
  (keymap-set global-map "M-s g" 'consult-grep)
  (keymap-set global-map "M-s k" 'consult-keep-lines)
  (keymap-set global-map "M-s l" 'consult-line)
  (keymap-set global-map "M-s r" 'consult-ripgrep)
  (keymap-set global-map "M-s M-f" 'consult-locate)
  (keymap-set global-map "M-s M-g" 'consult-git-grep)
  (keymap-set global-map "M-s M-k" 'consult-focus-lines)
  (keymap-set global-map "M-s M-l" 'consult-line-multi)
  (keymap-set global-map "M-s M-o" 'consult-multi-occur)
  (keymap-set isearch-mode-map "M-s h" 'consult-isearch-history)
  (keymap-set global-map "M-s l" 'consult-line)
  (keymap-set global-map "M-s M-l" 'consult-line-multi))

(use-package embark
  :ensure t
  :pin gnu
  :autoload embark-prefix-help-command
  :commands embark-dwim embark-act
  :init
  (keymap-set global-map "C-;" 'embark-act)
  (keymap-set global-map "C-'" 'embark-dwim)

  ;; Replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :ensure t
  :pin gnu
  :after (embark consult)
  :demand t
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package simple
  :init
  ;; Do not saves duplicates in kill-ring
  (setq kill-do-not-save-duplicates t)

  ;; Save clipboard after quit.
  (setq save-interprogram-paste-before-kill t)

  ;; Enable better splitting words support of CJK characters.
  (setq word-wrap-by-category t)

  ;; Hide commands in M-x which do not work in the current mode.
  ;; Vertico commands are hidden in normal buffers.
  (setq read-extended-command-predicate
        #'command-completion-default-include-p)
  :hook
  (emacs-startup-hook . column-number-mode)
  (emacs-startup-hook . line-number-mode)
  (emacs-startup-hook . size-indication-mode))

(use-package mouse
  :init
  ;; Allow drag current region when press Control key.
  (setq mouse-drag-and-drop-region 'control)

  ;; mouse yank commands yank at point instead of at click.
  (setq mouse-yank-at-point t)
  :hook (emacs-startup-hook . context-menu-mode))

(use-package mwheel
  :init
  ;; Move only two lines when mouse wheel is turned.
  (setq mouse-wheel-scroll-amount '(2 ((shift) . hscroll))
        mouse-wheel-scroll-amount-horizontal 2))

(use-package pixel-scroll
  :hook
  (emacs-startup-hook . pixel-scroll-mode)
  (emacs-startup-hook . pixel-scroll-precision-mode))

(use-package cursor-sensor
  :hook (minibuffer-setup-hook . cursor-intangible-mode))

(use-package uniquify
  :init
  ;; Use forward style buffer name.
  (setq uniquify-buffer-name-style 'forward))

(use-package files
  :init
  ;; Enable Auto-saving.
  (setq auto-save-default t)

  ;; Don't auto-disable auto-save after deleting big chunks.  This
  ;; defeats the purpose of a failsafe.
  (setq auto-save-include-big-deletions t)

  ;; Change default transforms to apply to buffer file name before
  ;; making auto-save file name.
  (setq auto-save-file-name-transforms
        (append `((".*"
                   ,(expand-file-name "auto-save/" my-data-directory)
                   t))
                auto-save-file-name-transforms))

  ;; Create backup files for each modified file on saving, and enable
  ;; make multiple numbered backup files.
  (setq make-backup-files t
        version-control t)

  ;; Make backup files using the old file replication.
  (setq backup-by-copying t)

  ;; Adjust the threshold for automatic deletion of versioned backup
  ;; files.
  (setq delete-old-versions t
        kept-old-versions 5
        kept-new-versions 5)

  ;; Prevent create backup files in-place, alternative create them in
  ;; `my-data-directory'.
  (setq backup-directory-alist
        (append
         `(("." . ,(expand-file-name "backup/" my-data-directory)))
         backup-directory-alist)))

(use-package recentf
  :preface
  (define-advice recentf-load-list
      (:around (fn &rest args) silence-message)
    "Silencing load message."
    (cl-letf (((symbol-function #'message) #'ignore))
      (apply fn args)))

  (define-advice recentf-cleanup
      (:around (fn &rest args) silence-message)
    "Silencing clean up message."
    (cl-letf (((symbol-function #'message) #'ignore))
      (apply fn args)))
  :init
  ;; Redirect recent files records.
  (setq recentf-save-file
        (expand-file-name "recent.el" my-state-directory))
  :config
  ;; Avoid record `my-data-directory' and `my-cache-directory' files.
  (add-to-list 'recentf-exclude my-data-directory)
  (add-to-list 'recentf-exclude my-cache-directory)
  :hook (emacs-startup-hook . recentf-mode))

(use-package savehist
  :init
  ;; Redirect minibuffer history file.
  (setq savehist-file (expand-file-name "hist.el" my-state-directory))
  :config
  ;; Auto delete duplicated history.
  (setq history-delete-duplicates t)
  :hook (emacs-startup-hook . savehist-mode))

(use-package saveplace
  :init
  ;; Redirect place file.
  (setq save-place-file
        (expand-file-name "place.el" my-state-directory))
  :hook (emacs-startup-hook . save-place-mode))

(use-package delsel
  :hook
  ;; Replace content in marked region with new text.
  (emacs-startup-hook . delete-selection-mode))

(use-package bookmark
  :config
  (setq bookmark-default-file
        (expand-file-name "bookmarks.el" my-data-directory)))

(use-package tramp
  :demand t
  :init
  ;; Only show connection errors.
  (setq tramp-verbose 1))

(use-package tramp-cache
  :init
  (setq tramp-persistency-file-name
        (expand-file-name "tramp.el" my-state-directory)))

(use-package diff-hl
  :ensure t
  :pin gnu
  :commands (diff-hl-mode
             diff-hl-dir-mode
             diff-hl-dired-mode
             diff-hl-flydiff-mode)
  :autoload (diff-hl-magit-pre-refresh
             diff-hl-magit-post-refresh)
  :hook
  (find-file-hook . diff-hl-mode)
  (vc-dir-mode-hook  . diff-hl-dir-mode)
  (dired-mode-hook   . diff-hl-dired-mode)
  (diff-hl-mode-hook . diff-hl-flydiff-mode))

(use-package svg-lib
  :ensure t
  :pin gnu
  :init
  ;; Redirect location of svg icons storage.
  (setq svg-lib-icons-dir
        (expand-file-name "svg-lib/" my-cache-directory)))

(use-package corfu
  :ensure t
  :pin gnu
  :commands corfu-mode corfu-history-mode
  :autoload (corfu--popup-show
             corfu--popup-hide
             corfu--popup-support-p)
  :preface
  (defun corfu-enable-in-minibuffer ()
    "Enable Corfu in the minibuffer if `completion-at-point' is bound."
    (when (where-is-internal #'completion-at-point
                             (list (current-local-map)))
      (corfu-mode +1)))
  :config
  ;; Enable cycling for `corfu-next' and `corfu-previous'.
  (setq corfu-cycle t)

  ;; Enable auto completion
  (setq corfu-auto t
        corfu-quit-no-match 'separator)

  ;; Disable candidate preselection.
  (setq corfu-preselect-first nil)

  ;; TAB-style keybindings.
  (keymap-set corfu-map "<tab>" 'corfu-next)
  (keymap-set corfu-map "<backtab>" 'corfu-previous)

  :hook
  (corfu-mode-hook . corfu-history-mode)
  (minibuffer-setup-hook . corfu-enable-in-minibuffer))

(use-package kind-icon
  :ensure t
  :pin gnu
  :after corfu
  :autoload kind-icon-margin-formatter
  :init
  (setq kind-icon-default-face 'corfu-default)
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(use-package corfu-doc
  :ensure t
  :pin melpa
  :when (display-graphic-p)
  :after corfu
  :init
  (keymap-set corfu-map "M-d" 'corfu-doc-toggle)
  (keymap-set corfu-map "M-n" 'corfu-doc-scroll-up)
  (keymap-set corfu-map "M-p" 'corfu-doc-scroll-down)
  :hook (corfu-mode-hook . corfu-doc-mode))

(use-package corfu-terminal
  :ensure t
  :pin nongnu
  :unless (display-graphic-p)
  :autoload (corfu-terminal--popup-show
             corfu-terminal--popup-hide
             corfu-terminal--popup-support-p)
  :preface
  (define-minor-mode my-corfu-terminal-mode
    "Corfu popup on terminal."
    :global nil
    :group 'corfu-terminal
    (if my-corfu-terminal-mode
        (progn
          (advice-add #'corfu--popup-show :around
                      #'corfu-terminal--popup-show)
          (advice-add #'corfu--popup-hide :around
                      #'corfu-terminal--popup-hide)
          (advice-add #'corfu--popup-support-p :override
                      #'corfu-terminal--popup-support-p))
      (advice-remove #'corfu--popup-show #'corfu-terminal--popup-show)
      (advice-remove #'corfu--popup-hide #'corfu-terminal--popup-hide)
      (advice-remove #'corfu--popup-support-p
                     #'corfu-terminal--popup-support-p)))
  :hook (corfu-mode-hook . my-corfu-terminal-mode))

(use-package prog-mode
  :hook
  ;; Highlight matched paren when programming.
  (prog-mode-hook . show-paren-mode)
  
  ;; Automatic parenthesis pairing when programming.
  (prog-mode-hook . electric-pair-mode)

  ;; Highlight current line when programming.
  (prog-mode-hook . hl-line-mode)

  ;; Display line numbers when programming.
  (prog-mode-hook . display-line-numbers-mode)

  ;; Show maximum fill column indicator.
  (prog-mode-hook . display-fill-column-indicator-mode)

  ;; Use `corfu' provides completion support.
  (prog-mode-hook . corfu-mode))

(use-package tree-sitter
  :ensure t
  :pin melpa
  :commands tree-sitter-mode)

(use-package xref
  :ensure consult
  :init
  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref))

(use-package flymake
  :config
  (keymap-set flymake-mode-map "C-c !" 'consult-flymake)

  ;; Always show flymake status and diagnostic in Mode Line.
  (when minions-mode
    (push 'flymake-mode minions-prominent-modes)))

(use-package eglot
  :ensure t
  :pin gnu
  :commands eglot-ensure
  :config
  ;; Auto kill `eglot' server after kill last modifed buffer.
  (setq eglot-autoshutdown t)

  ;; Remove `eglot' mode line status information.
  (setq mode-line-misc-info
        (delete `(eglot--managed-mode
                  (" [" eglot--mode-line-format "] "))
                mode-line-misc-info)))

(use-package cc-mode
  :hook
  (c-mode-hook . tree-sitter-mode)
  (c-mode-hook . eglot-ensure)
  (c++-mode-hook . tree-sitter-mode)
  (c++-mode-hook . eglot-ensure))

(use-package inspector
  :ensure t
  :pin gnu
  :commands (inspector-inspect-last-sexp inspector-inspect-expression))

(use-package pp
  :after elisp-mode
  :commands (pp-macroexpand-last-sexp pp-macroexpand-expression))

(use-package elisp-mode
  :config
  (keymap-set emacs-lisp-mode-map "C-c C-i" 'inspector-inspect-last-sexp)
  (keymap-set emacs-lisp-mode-map "C-c C-m" 'pp-macroexpand-last-sexp)
  (keymap-set emacs-lisp-mode-map "C-c M-i" 'inspector-inspect-expression)
  (keymap-set emacs-lisp-mode-map "C-c M-m" 'pp-macroexpand-expression)
  :hook
  (emacs-lisp-mode-hook . flymake-mode)
  ;; Inherit `load-path' when `flymake' byte compile.
  (emacs-lisp-mode-hook
   . (lambda ()
       (setq-local elisp-flymake-byte-compile-load-path load-path))))

(use-package haskell-mode
  :ensure t
  :pin nongnu
  :hook
  (haskell-mode-hook . tree-sitter-mode)
  (haskell-mode-hook . eglot-ensure))

(use-package rust-mode
  :ensure t
  :pin nongnu
  :hook
  (rust-mode-hook . tree-sitter-mode)
  (rust-mode-hook . eglot-ensure))

(use-package text-mode
  :hook
  ;; Highlight current line when edit text file.
  (text-mode-hook . hl-line-mode)

  ;; Auto wrap line when edit text file.
  (text-mode-hook . visual-line-mode))

(use-package auctex
  :ensure t
  :pin gnu
  :after tex
  :defines TeX-command-list
  :init
  (add-to-list 'TeX-command-list
     	       '("XeLaTeX"
                 "xelatex -interaction=nonstopmode %s"
		 TeX-run-command
                 t
                 t
                 :help "Run XeLaTex")
               'append))

(use-package org-modern
  :ensure t
  :pin gnu
  :config
  (setq org-modern-star '("§"))
  :commands (org-modern-mode org-modern-agenda))

(use-package org
  :ensure t
  :pin gnu
  :config
  ;; Hide *, ~ and / in org text.
  (setq org-hide-emphasis-markers t)

  ;; Ellipsis symbol.
  (setq org-ellipsis "…")

  ;; Show entities as UTF8 characters.
  (setq org-pretty-entities t)

  ;; Auto indent mode as default.
  ;; (setq org-startup-indented t)

  ;; Tags align
  (setq org-auto-align-tags nil
        org-tags-column 0)

  ;; Check invisible region.
  (setq org-catch-invisible-edits 'show-and-error)

  ;; Bring back the cursor to the beginning or end of the headline text.
  (setq org-special-ctrl-a/e t)

  ;; State keywords of `org-mode'.
  (setq org-todo-keywords
        '((sequence "TODO(t!)"
                    "WAIT(w!)"
                    "|"
                    "DONE(d)"
                    "CANCEL(c)")))
  :hook
  (org-mode-hook . org-modern-mode))

(use-package org-agenda
  :init
  (keymap-set global-map "C-x M-o a" 'org-agenda)
  :config
  (add-to-list 'org-agenda-files "~/org/agenda/inbox.org")
  (add-to-list 'org-agenda-files "~/org/agenda/gtd.org")
  (add-to-list 'org-agenda-files "~/org/agenda/tickler.org")

  ;; Tags align.
  (setq org-agenda-tags-column 0)

  (setq org-agenda-block-separator ?─)

  (setq org-agenda-time-grid
        '((daily today require-timed)
          (000 100 200 300 400 500 600 700 800 900 1000 1100 1200
           1300 1400 1500 1600 1700 1800 1900 2000 2100 2200 2300)
          " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄"))

  (setq org-agenda-current-time-string
        "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄ now ┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄")
  :hook (org-agenda-finalize-hook . org-modern-agenda))

(use-package org-capture
  :init
  (keymap-set global-map "C-x M-o c" 'org-capture)
  :config
  ;; Task.
  (add-to-list 'org-capture-templates
               '("t"
                 "Todo [inbox]"
                 entry
                 (file+headline "~/org/agenda/inbox.org" "Tasks"))
               'append)

  ;; Tickler
  (add-to-list 'org-capture-templates
               '("T"
                 "Tickler"
                 entry
                 (file+headline "~/org/agenda/tickler.org" "Tickler"))
               'append))

(use-package org-refile
  :config
  (add-to-list 'org-refile-targets '("~/org/agenda/gtd.org" :maxlevel . 3))
  (add-to-list 'org-refile-targets '("~/org/agenda/someday.org" :level . 1))
  (add-to-list 'org-refile-targets '("~/org/agenda/tickler.org" :maxlevel . 2)))

(use-package eshell
  :defines eshell-directory-name
  :init
  (setq eshell-directory-name
        (expand-file-name "eshell/" my-config-directory)))

(use-package transient
  :ensure t
  :pin melpa
  :init
  ;; Redirect `transient' cache files.
  (setq transient-history-file
        (expand-file-name "transient/history.el" my-state-directory)
        transient-levels-file
        (expand-file-name "transient/levels.el" my-state-directory)
        transient-values-file
        (expand-file-name "transient/values.el" my-state-directory)))

(use-package magit
  :ensure t
  :pin nongnu
  :commands (magit-dispatch magit-status)
  :defines (magit-pre-refresh-hook magit-post-refresh-hook)
  :init
  (setq magit-define-global-key-bindings nil)
  (keymap-set global-map "C-x M-v g" 'magit-dispatch)
  :hook
  (magit-pre-refresh-hook . diff-hl-magit-pre-refresh)
  (magit-post-refresh-hook . diff-hl-magit-post-refresh))

(use-package private
  :no-require t
  :when (file-exists-p (concat user-emacs-directory "private.el" ))
  :init
  (load (concat user-emacs-directory "private.el" ) nil 'nomessage))

(provide 'init)
;;; init.el ends here
