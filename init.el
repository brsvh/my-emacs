;;; init.el --- My Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2020-2021 Burgess Chang

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Start Emacs life from scratch :).

;; This file is the Emacs initialization file, it will be loading when
;; Emacs is loaded.  Use Emacs directly is like stuck in the mud, this
;; is the reason for further configuration of Emacs in this file.

;; This file is written in a literary-like manner, but no with real
;; literate programming.  It just because the performance issue of the
;; `org' package.

;; When you look at the contents of the parent directory of this file
;; (Emacs user directory) you will find it contains only a single init
;; file.  In the real world of Emacs, some people split their Emacs
;; configuration into multiple files, in other words - modularity.

;; About I choose to merge configuration in a single file is for
;; better performance, for the Emacs interpreter, each `require' or
;; `load' will slow down the startup speed of Emacs, although
;; somewhat negligible.

;; To see the outline of this file, run M-x occur with the following
;; query:
;;   ^;;;;* \|^(use-package

;;; Code:

;; I would like to use early initialization support, it requires Emacs
;; version greater than 27.1.  But I also have machine install Emacs
;; with "feature/native-comp" master, so I can't do Emacs version
;; check.
;; (when (version< emacs-version "27.1")
;;   (error "This requires Emacs 27.1 and above!"))

;;; Prologue:

;; My environment respect XDG user directory specification, make Emacs
;; do it too.  `xdg' provides XDG Base Directory Specification support
;; in Emacs, load it.
(require 'xdg)

;; Define some constants for directory will be used next.
(defconst my-dir user-emacs-directory
  "The root directory of my Emacs config.")

(defconst my-data-dir (expand-file-name "emacs/" (xdg-data-home))
  "Directory for my data storage.")

(defconst my-cache-dir (expand-file-name "emacs/" (xdg-cache-home))
  "Directory for my cache storage.")

;; Emacs will save customization to `custom-file', it is
;; `user-init-file' by default.  I don't want to it littering my
;; `user-init-file', redirect it.
(setq custom-file (concat my-dir "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file nil 'nomessage))

;; When use feature/native-comp branch, the native compilation is
;; supported.

;; Don't store eln cache in eln-cache under `user-emacs-directory'.
(when (boundp 'comp-eln-load-path)
  (push (concat my-cache-dir "eln-cache/") comp-eln-load-path))

;; `package.el' is Emacs built-in package manager, its default
;; behavior is a bit annoying.  Load it now.
;; (require 'package)

;; `package' will storage package cache in elpa/ directory under
;; `user-emacs-directory'.  It seems not good, move it to
;; `my-cache-dir'.
;; (setq package-user-dir (concat my-cache-dir "elpa/"))

;; Ensure `package-gnupghome-dir' is under `package-user-dir'.
;; (setq package-gnupghome-dir (concat package-user-dir "gnupg/"))

;; The archives of `package' only include GNU elpa by default, set
;; package archive to include ELPA, MELPA and Org's ELPA.
;; (setq package-archives '(("gnu"   . "https://elpa.gnu.org/packages/")
;;                          ("melpa" . "https://melpa.org/packages/")
;;                          ("org"   . "https://orgmode.org/elpa/")))

;; `package.el' supports ahead-of-time native compilation when
;; installing a package, activate it.
;; (when (boundp 'package-native-compile)
;;   (setq package-native-compile t))

;; `package-initialization' will do byte-compilation and generate
;; autoload defs.
;; (package-initialize)

;; I use `use-package.el' to organize my Emacs configuration.
;; `use-package' provides `use-package' macro that allows me to
;; isolate package configuration by performance-oriented way.
;; (unless (package-installed-p 'use-package)
;;     (package-refresh-contents)
;;     (package-install 'use-package))
;; (require 'use-package)

;; `package' vs `straight'

;; `package' downloads pre-built packages from central servers using a
;; special HTTP protocol, while `straight' clones Git (or other)
;; repositories and builds packages locally.

;; `package' time usage is 0.3s~ on my machine, so migrate to
;; `straight'.

;; Get rid of assignment to free variables.
(defvar straight-base-dir)
(defvar straight-repository-branch)
(defvar straight-build-dir)
(defvar straight-vc-git-default-clone-depth)
(defvar straight-check-for-modifications)
(defvar my-straight-dir (concat my-cache-dir
				"straight/repos/straight.el/"))

;; Customize `straight' behaviors.
(setq straight-base-dir my-cache-dir
      straight-repository-branch "develop"
      straight-build-dir (format "build-%s/" emacs-version)
      straight-vc-git-default-clone-depth 1)

;; If watchexec and Python are installed, use file watchers to detect
;; package modifications. This saves time at startup. Otherwise, use
;; the ever-reliable find(1).
(if (and (executable-find "watchexec")
         (executable-find "python3"))
    (setq straight-check-for-modifications
          '(watch-files find-when-checking))
  (setq straight-check-for-modifications
        '(find-at-startup find-when-checking)))

;; Download `straight' if it is not installed.
(unless (file-directory-p my-straight-dir)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
       'silent 'inhibit-cookies)
    (goto-char (point-max))
    (eval-print-last-sexp)))

;; Add `my-straight-dir' to `load-path', and require `straight'.
(push my-straight-dir load-path)
(require 'straight)

;; packages bootstrap.
(with-temp-buffer
  (insert-file-contents (concat my-straight-dir "bootstrap.el"))
  (eval-region (search-forward "(require 'straight)") (point-max)))

;; Ensure `use-package' is installed and loaded.
(straight-use-package 'use-package)
(require 'use-package)

;; Customize the default behaviors of `use-package'.
(use-package use-package
  :custom
  ;; Not use `use-package-always-defer'.

  ;; According to the follow description in `use-package'
  ;; documentation.

  ;; NOTE: pay attention if you set `use-package-always-defer' to t,
  ;; and also use the ":after" keyword, as you will need to specify
  ;; how the declared package is to be loaded: e.g., by some ":bind".
  ;; If you're not using one of the mechanisms that registers
  ;; autoloads, such as ":bind" or ":hook", and your package manager
  ;; does not provide autoloads, it's possible that without adding
  ;; ":demand t" to those declarations, your package will never be
  ;; loaded.

  ;; In almost all cases you don’t need to manually specify :defer t.
  ;; This is implied whenever ":bind" or ":mode" or ":interpreter" is
  ;; used.  Typically, you only need to specify ":defer" if you know
  ;; for a fact that some other package will do something to cause
  ;; your package to load at the appropriate time, and thus you would
  ;; like to defer loading even though use-package isn’t creating any
  ;; autoloads for you.

  ;; So I will only manually specified ":defer" in some extreme cases.
  (use-package-always-defer nil)
  ;; Use the real name of hooks.
  
  ;; By default, when using ":hook" omit the "-hook" suffix if you
  ;; specify the hook explicitly, as this is appended by default.  For
  ;; example the following code will not work as it attempts to add to
  ;; the `prog-mode-hook-hook' which does not exist.  This
  ;; simplification can lead to bad habits, so use the real name of
  ;; hooks instead of a shorter version: after-init ==>
  ;; `after-init-hook'.
  (use-package-hook-name-suffix nil)
  ;; Some other useful settings.
  (use-package-compute-statistics   nil)
  (use-package-enable-imenu-support t)
  (use-package-expand-minimally     nil))

;; Register some useful keywords of `use-package'.
;; ":blackout", ":genreal" and ":hydra".
(use-package blackout
  :straight t)

(use-package general
  :straight t)

(use-package hydra
  :straight t)

(use-package use-package-hydra
  :straight t)

;;; Better default:

;; Emacs have a powerful encoding system to deal with many coding
;; systems to particular regions, but I want use UTF-8 as default.
(setq locale-coding-system 'utf-8)
(when (fboundp 'set-charset-priority)
  (set-charset-priority 'unicode))
(prefer-coding-system 'utf-8)

;; I have deferred garbage collection further back in the startup
;; process.  Revert to garbage collection with `gcmh' after Emacs
;; startup.  `gcmh' is a package provides magic hack of garbage
;; collector.
(use-package gcmh
  :straight t
  :blackout (gcmh-mode)
  :hook (after-init-hook . gcmh-mode)
  :custom
  (gcmh-verbose             t)
  (gcmh-lows-cons-threshold #x800000)
  (gcmh-high-cons-threshold most-positive-fixnum)
  (gcmh-idle-delay          30))

;; Confirm with `y-or-n-p' when exit Emacs.
(setq confirm-kill-emacs 'y-or-n-p)

;; Start Emacs server after startup.
(use-package server
  :hook (after-init-hook . server-mode))

;; I don't need suspend Emacs, so inhibit the keybinding.
(global-unset-key (kbd "C-z"))

;; `y-or-n-p' is one way that Emacs asks a yes-or-no question, usually
;; to get your confirmation for something you requested.

;; `yes-or-no-p' is another way that Emacs uses to do this.  It is
;; typically used for more important yes-or-no questions, that is,
;; questions where the answer might have more serious consequences.
;; You must type a full yes or no to answer `yes-or-no-p'.

;; By default, Emacs use the relatively conservative way (yes-or-no-p)
;; to confirm answer.  I want to always use `y-or-n-p'.
(fset 'yes-or-no-p 'y-or-n-p)

;; Allow open minibuffer by run command in exists minibuffer.
(setq enable-recursive-minibuffers t)

;; Change default auto save and backup settings.
(setq create-lockfiles nil
      make-backup-files nil
      auto-save-default t)

(setq backup-by-copying t
      backup-directory-alist `((".*" .
                                ,(concat my-data-dir "backup/"))))

(setq auto-save-file-name-transforms `((".*"
                                        ,(concat my-cache-dir
                                                 "auto-save/") t))
      auto-save-list-file-prefix (concat my-cache-dir "auto-save/"))

(setq version-control t
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2)

;; Don’t litter user-emacs-directory!
(use-package no-littering
  :straight t
  :demand t
  :defines (no-littering-etc-directory
            no-littering-var-directory)
  :functions (no-littering-expand-var-file-name
              no-littering-expand-etc-file-name)
  :init
  (setq no-littering-etc-directory my-data-dir
        no-littering-var-directory my-cache-dir))

;; Use `super-save-mode' to save all buffers.
(use-package super-save
  :straight t
  :blackout (super-save-mode)
  :custom
  ;; Enable the additional feature of auto-saving buffers when Emacs
  ;; is idle.
  (super-save-auto-save-when-idle t)
  ;; Turn off super-save for remote files.
  (super-save-remote-files nil)
  ;; Exclude specific files from super-save.
  (super-save-exclude '(".gpg")))

;; `recentf-mode' is a built-in minor mode that keeps track of the
;; files you have opened, allowing you to revisit them faster.  Its
;; true power consists in the fact that its data, maintained in
;; recentf-list, is a simple variable.  This means that we can access
;; it through any relevant piece of Elisp functionality.
(use-package recentf
  :blackout (recentf-mode)
  :hook (after-init-hook . recentf-mode)
  :custom
  (recentf-max-saved-items 500)
  (recentf-max-menu-items 15)
  (recentf-auto-cleanup 'never)
  (recentf-exclude '(my-data-dir
                     my-cache-dir
                     ".cache"
                     ".cask"
                     ".elfeed"
                     "bookmarks"
                     "cache"
                     "ido.*"
                     "persp-confs"
                     "recentf"
                     "undo-tree-hist"
                     "url"
                     "COMMIT_EDITMSG\\'")))

;; When we need reuse Emacs, restore cursor position is useful.
;; `save-place-mode' can remember where the point is in any given
;; file.  This can often be a subtle reminder of what you were doing
;; the last time you visited that file, allowing you to pick up from
;; there.
(use-package saveplace
  :blackout (save-place-mode)
  :hook (after-init-hook . save-place-mode)
  :custom
  (save-place-forget-unreadable-files t))

;; Emacs' splash screen is enable by default, even if it’s started
;; with a argument, like opening a file.  So the solution was inhibit
;; the startup screen
(setq inhibit-startup-screen            t
      inhibit-startup-message           t
      inhibit-startup-echo-area-message t
      initial-scratch-message           nil
      initial-major-mode                'fundamental-mode)

;; When two buffers have the same name, Emacs will try to disambiguate
;; them by displaying their element of differentiation in accordance
;; with the style of `uniquify-buffer-name-style'.  While
;; `uniquify-strip-common-suffix' will remove the part of the file
;; system path they have in common.
(use-package uniquify
  :custom
  (uniquify-buffer-name-style 'forward)
  (uniquify-strip-common-suffix t)
  (uniquify-after-kill-buffer-p t))

;; Get rid of assignment to free variable `shackle-rules'.
(defvar shackle-rules '())

;; Define some popup windows manage rules by use `shackle.el'.
;; This block borrowed from Centaur Emacs.
(use-package shackle
  :straight t
  :blackout t
  :preface
  (defvar shackle--popup-window-list nil)
  
  (defvar-local shackle--current-popup-window nil)
  
  (defun shackle-display-buffer-hack (fn buffer alist plist)
    (let ((window (funcall fn buffer alist plist)))
      (setq shackle--current-popup-window window)

      (when (plist-get plist :autoclose)
        (push (cons window buffer) shackle--popup-window-list))
      window))

  (defun shackle-close-popup-window-hack (&rest _)
    "Close current popup window via `C-g'."
    (setq shackle--popup-window-list
          (cl-loop for (window . buffer) in shackle--popup-window-list
                   if (and (window-live-p window)
                           (equal (window-buffer window) buffer))
                   collect (cons window buffer)))
    (when (and (called-interactively-p 'interactive)
               (not (region-active-p)))
      (let (window buffer)
        (if (one-window-p)
            (progn
              (setq window (selected-window))
              (when (equal (buffer-local-value 'shackle--current-popup-window
                                               (window-buffer window))
                           window)
                (winner-undo)))
          (setq window (caar shackle--popup-window-list))
          (setq buffer (cdar shackle--popup-window-list))
          (when (and (window-live-p window)
                     (equal (window-buffer window) buffer))
            (delete-window window)
            (pop shackle--popup-window-list))))))
  :hook (after-init-hook . shackle-mode)
  :commands shackle-display-buffer
  :custom
  (shackle-default-size 0.5)
  (shackle-default-alignment 'below)
  (shackle-default-rule nil)
  (shackle-rules '(("*Backtrace*"   :size 0.7 :align 'below :autoclose t :select t)
                   ("*Compile-Log*" :size 0.5 :align 'below :autoclose t)
                   ("*Error*"       :size 0.5 :align 'below :autoclose t)
                   ("*Help*"        :size 0.5 :align 'below :autoclose t)
                   ("*Warnings*"    :size 0.5 :align 'below :autoclose t)
                   ("*Messages*"    :size 0.5 :align 'below :autoclose t)))
  :init
  (put 'shackle--current-popup-window 'permanent-local t)
  (advice-add #'keyboard-quit :before #'shackle-close-popup-window-hack)
  (advice-add #'shackle-display-buffer :around #'shackle-display-buffer-hack))

;; By default, Emacs displays the current line number of the point in
;; the mode line.  In addition, display the current column is also
;; useful.
(setq line-number-mode     t
      column-number-mode   t
      size-indication-mode t)

;; By default, Emacs inserts tabs in place of multiple spaces when it
;; formats a region.  But tab symbol could not display uniformly on
;; all machines, so I turn off Indent Tabs mode.
(setq fill-column 70)
(setq indent-tabs-mode nil)

;; About auto indent, Emacs use electric-indent-mode to
;; automatically indents the line after every RET typed.  I want to
;; enable it only when I need.
(setq electric-indent-inhibit t)

;; This makes the visual region behave more like the contemporary
;; concept of highlighted text, that can be erased or overwritten as a
;; whole.
(setq delete-selection-mode t)

;;; Interface Enhancement:

;; `prescient-persist-mode' provides usage statistics to be saved
;; between Emacs sessions.
(use-package prescient
  :straight t
  :blackout t
  :hook (after-init-hook . prescient-persist-mode)
  :custom
  (prescient-save-file (concat my-cache-dir "prescient-save.el")))

;; `winner-mode' is a global minor mode, allow undo or redo changes in
;; the window configuration.
(use-package winner
  :blackout (winner)
  :general
  (:prefix "C-c e"
           ""        '(:ignore t :which-key "edit")
           "<left>"  '(winner-undo :which-key "undo")
           "<right>" '(winner-redo :which-key "redo"))
  :hook (after-init-hook . winner-mode)
  :custom
  (winner-dont-bind-my-keys t))

;; Standard Emacs command `occur' lists all lines of the current
;; buffer that match a regexp that you give it.  The matching lines
;; are listed in buffer *Occur*, and you can click them there to
;; navigate to the corresponding lines in the original buffer.
(use-package replace
  :commands occur
  :config
  (push '(occur-mode :select t :size 0.5 :align 'below :autoclose t)
        shackle-rules))

;;; Navigation:

;; By default, Emacs will goto the beginning of line when press
;; C-a. `mwim' provides better cursor jump experience.
(use-package mwim
  :straight t
  :general
  ("C-a"    'mwim-beginning-of-code-or-line)
  ("C-e"    'mwim-end-of-code-or-line)
  ("<home>" 'mwim-beginning-of-code-or-line)
  ("<end>"  'mwim-end-of-code-or-line))

;;; Visual:

;; Hightlight current line globally.
(use-package hl-line
  :blackout (global-hl-line-mode hl-line-mode)
  :hook (after-init-hook . global-hl-line-mode))

;;; Project management:

;; `projectile' is a project interaction library for Emacs.  Its goal
;; is to provide a nice set of features operating on a project level
;; without introducing external dependencies (when feasible).  For
;; instance - finding project files has a portable implementation
;; written in pure Emacs Lisp without the use of GNU find (but for
;; performance sake an indexing mechanism backed by external commands
;; exists as well).
(use-package projectile
  :straight t
  :blackout t
  :hook (after-init-hook . projectile-mode)
  :general
  ("C-c p" '(:keymap projectile-command-map
             :which-key "+project")))

;;; Programming:

;; Smartparens is a minor mode for dealing with pairs in Emacs, it can
;; automatically insert pairs.
(use-package smartparens
  :straight t
  :blackout t
  :hook
  (after-init-hook . smartparens-global-mode)
  (smartparens-mode-hook . show-smartparens-mode)
  :commands (sp-pair
             sp-local-pair
             sp-with-modes
             sp-point-in-comment
             sp-point-in-string)
  :config
  (require 'smartparens-config)
  (sp-pair "=" "=" :actions '(wrap))
  (sp-pair "+" "+" :actions '(wrap))
  (sp-pair "<" ">" :actions '(wrap))
  (sp-pair "$" "$" :actions '(wrap))
  (sp-pair "（" "）" :actions '(insert wrap autoskip navigate))
  (sp-pair "“" "”" :actions '(insert wrap autoskip navigate))
  (sp-pair "‘" "’" :actions '(insert wrap autoskip navigate))
  (sp-pair "《" "》" :actions '(insert wrap autoskip navigate))
  (sp-pair "「" "」" :actions '(insert wrap autoskip navigate))
  (sp-pair "『" "』" :actions '(insert wrap autoskip navigate)))

;;; Completion:

;; Abbrevs is fast insert method, defined abbrev is a word which
;; expands, if you insert it, into some different text.  Abbrevs are
;; defined by the user to expand in specific ways.  I don't want to
;; use it, so inhibit it.
(use-package abbrev
  :blackout (abbrev-mode)
  :init
  (setq-default abbrev-mode t)
  :custom
  (save-abbrevs 'silent))

;; Complete anything ?!
(use-package company
  :straight t
  :blackout t
  :defer t
  :functions (company-dabbrev-ignore-case company-dabbrev-downcase)
  :commands company-cancel
  :hook (after-init-hook . global-company-mode)
  :general
  (:keymaps 'company-active-map
            "C-p"   'company-select-previous
            "C-n"   'company-select-next
            "<tab>" 'company-complete-common-or-cycle)
  (:keymaps 'company-search-map
            "C-p"   'company-select-previous
            "C-n"   'company-select-next)
  :custom
  (company-backends '(company-capf
                     (company-dabbrev-code company-keywords company-files)
                     company-dabbrev))
  (company-dabbrev-downcase nil)
  (company-dabbrev-ignore-case nil)
  (company-echo-delay (if (display-graphic-p) nil 0))
  (company-global-modes '(not erc-mode message-mode help-mode
                              gud-mode eshell-mode shell-mode))
  (company-idle-delay 0)
  (company-minimum-prefix-length 2)
  (company-require-match nil)
  (company-tooltip-align-annotations t)
  (company-tooltip-limit 12))

;; Documentation popups support for `company'.
(use-package company-quickhelp
  :straight t
  :hook (company-mode-hook . company-quickhelp-mode))

;;; Error Checking:

;; `flycheck' provides syntax checking for Emacs, yet a more modern
;; replacement to `flymake'.
(use-package flycheck
  :straight t
  :blackout (flycheck-mode global-flycheck-mode)
  :general
  ("C-c c" '(:which-key "check"))
  :hook (emacs-startup-hook . global-flycheck-mode)
  :custom
  (flycheck-keymap-prefix (kbd "C-c c"))
  (flycheck-display-errors-delay 0)
  (flycheck-global-modes '(not text-mode
                               outline-mode
                               fundamental-mode
                               lisp-interaction-mode
                               org-mode
                               diff-mode
                               shell-mode
                               eshell-mode
                               term-mode
                               vterm-mode))
  (flycheck-emacs-lisp-load-path 'inherit)
  (flycheck-indication-mode (if (display-graphic-p)
                                'right-fringe
                              'right-margin)))

;; Key Cheat Sheet:

;; Emacs keybinding system is powerful, we can bind thousands of
;; special behavior with key or keympas.  But sometimes I will forget
;; my settings, `which-key' support display available keybindings in
;; popup buffer.
(use-package which-key
  :straight t
  :defer t
  :blackout t
  :hook (after-init-hook . which-key-mode)
  :custom
  (which-key-show-transient-maps  t)
  (which-key-show-early-on-C-h    t)
  (which-key-idle-delay           most-positive-fixnum)
  (which-key-idle-secondary-delay 1e-100))

;;; Config Management:

;; Emacs Start Up Profiler.  Benchmark Emacs Startup time without ever
;; leaving Emacs.
(use-package esup
  :straight t
  :commands esup
  :custom
  (esup-depth 0)
  :config
  (push '("*esup*" :same t) shackle-rules))

;;; Appearance:

;; `mood-line' is a minimal mode-line configuration that aims to
;; replicate some of the features of the `doom-modeline' package.
(use-package mood-line
  :straight t
  :blackout (mood-line-mode)
  :hook (after-init-hook . mood-line-mode)
  :custom
  (mood-line-show-eol-style            t)
  (mood-line-show-encoding-information t))

;;; Theme:

;; The Modus themes are designed for accessible readability.  They
;; conform with the highest standard for color contrast between any
;; given combination of background and foreground values.  This
;; corresponds to the WCAG AAA standard, which specifies a minimum
;; rate of distance in relative luminance of 7:1.
(use-package modus-themes
  :straight t
  ;; Why defer this package 0.5s?

  ;; I should do theme settings in early initialization, but
  ;; `straight' is not initialize during early initialization.
  
  ;; Load theme during initialization is expensive, so defer it 0.5s.
  :defer 0.5
  :config
  ;; Customizations of modus theme.
  (setq modus-themes-slanted-constructs t
        modus-themes-bold-constructs    t
        modus-themes-mode-line          '3d)

  ;; Load the theme files before enabling a theme.
  (modus-themes-load-themes)
  (modus-themes-load-operandi))

(provide 'init)
;;; init.el ends here
