;;; my-ui.el --- UI enhancements of My Emacs -*- lexical-binding: t -*-

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

;; Essential libraies:

(use-my lib
  :keymap-set
  (:ctl-c-e-map
   ("t" . my/toggle-theme))
  :hook
  (on-init-ui-hook . my-theme-setup))

(use-package nerd-icons
  :vc (:url "https://github.com/rainstormstudio/nerd-icons.el.git"))



;; Builtin interface:

(use-package emacs
  :no-require t
  :init
  (menu-bar-mode -1)
  (tool-bar-mode -1)

  (setq frame-resize-pixelwise t
        window-resize-pixelwise t)

  (setq auto-window-vscroll nil
        fast-but-imprecise-scrolling t)

  (setq scroll-preserve-screen-position t
        scroll-margin 0
        scroll-conservatively 101)

  (setq hscroll-margin 2
        hscroll-step 1)

  (setq enable-recursive-minibuffers t)

  (setq history-delete-duplicates t)

  (setq inhibit-compacting-font-caches t))

(use-package scroll-bar
  :when (display-graphic-p)
  :init
  (scroll-bar-mode -1))

(use-package startup
  :no-require t
  :init
  (setq inhibit-startup-screen t
        inhibit-startup-echo-area-message t
        initial-scratch-message nil
    initial-major-mode 'fundamental-mode))

(use-package winner
  :keymap-set
  (:ctl-c-map
   ("<left>" . winner-undo)
   ("<right>" . winner-undo))
  :config
  (-snocq winner-boring-buffers
          "*Backtrace*"
          "*Compile-Log*"
          "*Help*"
          "*Warnings*")
  :hook
  (on-init-ui-hook . winner-mode))

(use-package popper
  :ensure popper
  :pin gnu
  :demand t
  :keymap-set
  ("C-`" . popper-toggle)
  ("C-~" . popper-cycle)
  ("M-p" . popper-toggle-type)
  :config
  (-snocq popper-reference-buffers
          "\\*Backtrace\\*"
          "\\*Compile-Log\\*"
          "\\*Help\\*"
          "\\*Warnings\\*")

  (defun my-popper-fit-window-height (win)
    "Determine the height of WIN by fitting it to the buffer's content."
    (fit-window-to-buffer)
    win
    (floor (frame-height) 3)
    (floor (frame-height) 3))

  (setq popper-window-height #'my-popper-fit-window-height)

  (define-advice keyboard-quit (:before (&rest _) close-popper-window)
    "Close popper window via `C-g'."
    (when (and (called-interactively-p 'interactive)
            (not (region-active-p))
            popper-open-popup-alist)
      (let ((window (caar popper-open-popup-alist)))
        (when (window-live-p window)
          (delete-window window)))))

  (with-eval-after-load 'doom-modeline
    (setq popper-mode-line
          '(:eval (let ((face (if (doom-modeline--active)
                                  'doom-modeline-emphasis
                                'doom-modeline)))
                    (if (and (icons-displayable-p)
                             (bound-and-true-p doom-modeline-icon)
                             (bound-and-true-p doom-modeline-mode))
                        (format " %s "
                                (nerd-icons-octicon "nf-oct-pin" :face face))
                      (propertize " POP " 'face face))))))
  :hook
  (on-first-buffer-hook . popper-mode)
  (on-first-buffer-hook . popper-echo-mode))

(use-package switch-window
  :vc (:url "https://github.com/dimitri/switch-window.git")
  :keymap-set
  ("<remap> <other-window>" . switch-window)
  ("<remap> <delete-other-windows>" . switch-window-then-maximize)
  ("<remap> <split-window-below>" . switch-window-then-split-below)
  ("<remap> <split-window-right>" . switch-window-then-split-right))

(use-package tab-bar
  :config
  (setq tab-bar-show 1
        tab-bar-new-tab-choice "*scratch*")
  :hook
  (my-init-hook . tab-bar-mode))



;; Themes:

(use-package modus-themes
  :ensure modus-themes
  :pin gnu
  :config
  (setq modus-themes-custom-auto-reload t)

  (setq modus-themes-bold-constructs t
        modus-themes-italic-constructs t)

  (setq modus-themes-mixed-fonts t)

  (setq modus-themes-prompts '(bold))

  (setq modus-themes-completions '((matches . (extrabold))
                                   (selection . (semibold
                                                 fitalic
                                                 text-also))))

  (setq modus-themes-org-blocks 'tinted-background)

  (setq modus-themes-headings '((0 . (1.40 ultrabold))
                                (1 . (1.30 extrabold))
                                (2 . (1.20 heavy))
                                (3 . (1.10 bold))
                                (t . (1.05 semibold))))

  (-snocq modus-themes-common-palette-overrides
          '(bg-tab-bar bg-active)
          '(bg-tab-current bg-main)
          '(bg-tab-other bg-inactive)
          '(border-mode-line-active unspecified)
          '(border-mode-line-inactive unspecified)))

(defun my--modus-themes-enale-p ()
  "Return t if current theme is belong to Modus Themes, else nil."
  (cl-some #'(lambda (theme)
               (member theme '(modus-operandi
                               modus-operandi-tinted
                               modus-vivendi
                               modus-vivendi-tinted)))
           custom-enabled-themes))

(defun my--reset-modus-themes-line-number-face ()
  "Use the more subtle line number background color."
  (when (my--modus-themes-enale-p)
    (let* ((cline 'line-number-current-line)
           (oline 'line-number)
           (proper-bg (face-attribute oline :background)))
      (set-face-attribute cline nil :background proper-bg))))

(defun my--reset-modus-themes-line-number-face-when-highlight ()
  "Use the more subtle line number background color."
  (when (my--modus-themes-enale-p)
    (if (bound-and-true-p hl-line-mode)
      (let* ((cline 'line-number-current-line)
             (hline 'hl-line)
             (oline 'line-number)
             (origin-bg (face-attribute oline :background))
             (proper-bg (face-attribute hline :background)))
       (if hl-line-mode
           (setq proper-bg (face-attribute hline :background))
         (setq proper-bg origin-bg))
       (set-face-attribute cline nil :background proper-bg))
      (my--reset-modus-themes-line-number-face))))

(use-package display-line-numbers
  :hook
  (display-line-numbers-mode-hook
   .
   my--reset-modus-themes-line-number-face))

(use-package solaire-mode
  :vc (:url "https://github.com/hlissner/emacs-solaire-mode.git")
  :config
  (-snocq solaire-mode-themes-to-face-swap "^modus-")
  :hook
  (on-init-ui-hook . solaire-global-mode))

(use-package hl-line
  :hook
  (hl-line-mode-hook
   .
   my--reset-modus-themes-line-number-face-when-highlight))



;; Mode line:

(use-package doom-modeline
  :vc (:url "https://github.com/seagle0128/doom-modeline.git")
  :config
  (setq doom-modeline-support-imenu t)

  (setq doom-modeline-window-width-limit 80)

  (setq doom-modeline-icon (display-graphic-p))

  (setq doom-modeline-buffer-state-icon nil)

  (setq doom-modeline-enable-word-count t
        doom-modeline-continuous-word-count-modes
        '(gfm-mode
          markdown-mode
          org-mode))

  ;; Whether display the indentation information.
  (setq doom-modeline-indent-info t)

  :hook
  (on-init-ui-hook . doom-modeline-mode))



;; Input completion:

(use-package vertico
  :ensure vertico
  :pin gnu
  :config
  (setq vertico-resize t)

  (setq vertico-cycle t)
  :hook
  (on-init-ui-hook . vertico-mode))

(use-package vertico-directory
  :ensure vertico
  :pin gnu
  :after vertico
  :keymap-set
  (:vertico-map
   ("<return>" . vertico-directory-enter)
   ("<backspace>" . vertico-directory-delete-char)
   ("M-<backspace>" . vertico-directory-delete-word)))

(use-package vertico-mouse
  :ensure vertico
  :pin gnu
  :after vertico
  :hook
  (vertico-mode-hook . vertico-mouse-mode))

(use-package minibuffer
  :after vertico
  :config
  (defun my--orderless-consult-suffix ()
    "Regexp which matches the end of string with Consult tofu support."
    (if (and (boundp 'consult--tofu-char) (boundp 'consult--tofu-range))
        (format "[%c-%c]*$"
                consult--tofu-char
                (+ consult--tofu-char consult--tofu-range -1))
      "$"))

  ;; Recognizes the following patterns:
  ;; * .ext (file extension)
  ;; * regexp$ (regexp matching at end)
  (defun my-orderless-consult-dispatch (word _index _total)
    (cond
     ;; Ensure that $ works with Consult commands, which add
     ;; disambiguation suffixes.
     ((string-suffix-p "$" word)
      `(orderless-regexp
        .
        ,(concat (substring word 0 -1) (my--orderless-consult-suffix))))
     ;; File extensions
     ((and (or minibuffer-completing-file-name
               (derived-mode-p 'eshell-mode))
           (string-match-p "\\`\\.." word)
       `(orderless-regexp . ,(concat "\\."
                                     (substring word 1)
                                     (my--orderless-consult-suffix)))))))

  (orderless-define-completion-style my-orderless-with-initialism
    (orderless-matching-styles '(orderless-initialism
                                 orderless-literal
                                 orderless-regexp)))

  (setq completion-in-region-function #'consult-completion-in-region)

  (setq completion-styles
        '(substring orderless basic partial-completion))

  (setq completion-category-overrides
        '((file (styles basic partial-completion))
          (command (styles my-orderless-with-initialism))
          (variable (styles my-orderless-with-initialism))
          (symbol (styles my-orderless-with-initialism))))

  (setq orderless-component-separator
        #'orderless-escapable-split-on-space)

  (setq orderless-style-dispatchers
        '(my-orderless-consult-dispatch
          orderless-affix-dispatch)))

(use-package marginalia
  :ensure marginalia
  :pin gnu
  :config
  (setq marginalia-align 'right)
  :hook
  (on-init-ui-hook . marginalia-mode))

(use-package savehist
  :config
  (setq savehist-file (my-state-path "hist.el"))
  :hook
  (on-init-ui-hook . savehist-mode))



;; Actions:

(use-package embark
  :ensure embark
  :pin gnu
  :keymap-set
  (("C-." . embark-act)
   ("C-;" . embark-dwim)
   ("C-h B" . embark-bindings))
  :init
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  (-snocq popper-reference-buffers
          "\\`\\*Embark Collect \\(Live\\|Completions\\)\\*")

  (with-eval-after-load 'winner
    (-snocq winner-boring-buffers
            "*Embark Collect Live*"
            "*Embark Collect Compiletions*"))

  (setq embark-verbose-indicator-display-action
        '(display-buffer-reuse-window display-buffer-below-selected)))

(use-package embark-consult
  :ensure embark-consult
  :pin gnu
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package activities
  :ensure activities
  :pin gnu
  :preface
  (defvar ctl-x-ctl-a-map (make-keymap)
    "Default keymap for C-c commands.")

  (keymap-set ctl-x-map "C-a" ctl-x-ctl-a-map)
  :keymap-set
  (:ctl-x-ctl-a-map
   ("C-a" . activities-resume)
   ("C-d" . activities-discard)
   ("C-k" . activities-kill)
   ("C-n" . activities-new)
   ("C-s" . activities-suspend)
   ("RET" . activities-switch)
   ("b" . activities-switch-buffer)
   ("g" . activities-revert)
   ("l" . activities-list))
  :hook
  (on-init-ui-hook . activities-mode)
  (on-init-ui-hook . activities-tabs-mode))

(use-package frameshot
  :vc (:url "https://github.com/tarsius/frameshot.git")
  :keymap-set
  (:ctl-c-5-map
   ("s" . frameshot-take)))



;; Builtin features:

(use-package bookmark
  :config
  (setq bookmark-default-file (my-data-path "bookmarks.el")))

(use-package info
  :keymap-set
  ("<remap> <info>" . consult-info))

(use-package uniquify
  :config
  (setq uniquify-buffer-name-style 'forward))



;; Third-party features:

(use-package anzu
  :ensure anzu
  :pin nongnu
  :hook
  (on-first-buffer-hook . global-anzu-mode))

(use-package hl-todo
  :vc (:url "https://github.com/tarsius/hl-todo.git")
  :hook
  (on-first-buffer-hook . global-hl-todo-mode))

(use-package rainbow-mode
  :ensure rainbow-mode
  :pin gnu
  :hook
  (find-file-hook . rainbow-mode))

(use-package page-break-lines
  :vc (:url "https://github.com/purcell/page-break-lines.git")
  :hook
  (on-init-ui-hook . global-page-break-lines-mode))

(provide 'my-ui)
;;; my-ui.el ends here
