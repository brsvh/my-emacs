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

(defgroup my nil
  "My Emacs customize."
  :group 'emacs)

(defcustom my-icon (display-graphic-p)
  "Use icons or not."
  :group 'my
  :type  'boolean)

(defcustom my-posframe (display-graphic-p)
  "Use posframe or not."
  :group 'my
  :type 'boolean)

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

;; `gnutls' provides support for SSL/TLS connections, using the GnuTLS
;; library.
(with-eval-after-load 'gnutls
  ;; `use-package' does this for us normally.
  (eval-when-compile
    (require 'gnutls))

  ;; Do not allow insecure TLS connections.
  (setq gnutls-verify-error t)

  ;; Bump the required security level for TLS to an acceptably modern
  ;; value.
  (setq gnutls-min-prime-bits 3072))

(defun my-no-query-on-http-kill (buffer)
  "Disable query-on-exit for all network connections in BUFFER.
This prevents Emacs shutdown from being interrupted just because
there is a pending network request."
    (prog1 buffer
      (set-process-query-on-exit-flag
       (get-buffer-process buffer) nil)))

;; `url-http' is a library for making HTTP requests.
(with-eval-after-load 'url-http
  (eval-when-compile
    (require 'url-http))
  (advice-add #'url-http :filter-return #'my-no-query-on-http-kill))

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
(require 'blackout)

(use-package general
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
(setq uniquify-buffer-name-style   'forward
      uniquify-strip-common-suffix t
      uniquify-after-kill-buffer-p t)

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

;; Inhibit display `visual-line-mode' in major/minor mode list.
(blackout visual-line-mode)

;; Normally when word-wrap is on, Emacs only breaks lines after
;; whitespace characters.  When this option is turned on, Emacs also
;; breaks lines after characters that have the "|" category (defined
;; in `characters.el').  This is useful for allowing breaking after
;; CJK characters and improves the word-wrapping for CJK text mixed
;; with Latin text. It only available on Emacs 28 and above.
(unless (version< emacs-version "28.0")
  (setq word-wrap-by-category t))

;;; Key-bindings:

(require 'general)
(general-define-key
 :prefix "C-c"
 "a" '(:ignore t :which-key "app")
 "b" '(:ignore t :which-key "buffer")
 "e" '(:ignore t :which-key "edit")
 "f" '(:ignore t :which-key "file")
 "g" '(:ignore t :which-key "goto")
 "h" '(:ignore t :which-key "help")
 "m" '(:ingore t :which-key "major")
 "v" '(:ignore t :which-key "vcs")
 "o" '(:ignore t :which-key "org")
 "p" '(:ignore t :which-key "project")
 "w" '(:ignore t :which-key "window")
 "W" '(:ignore t :which-key "workspace"))

;; `hydra' can be used to tie related commands into a family of short
;; bindings with a common prefix.
(use-package hydra
  :straight t)

;; `hydra-posframe' shows hydra hints on posframe.
(use-package hydra-posframe
  :when my-posframe
  :straight (hydra-posframe :type git
                            :host github
                            :repo "Ladicle/hydra-posframe")
  :hook (after-init-hook . hydra-posframe-mode)
  :custom
  (hydra-posframe-parameters '((left-fringe  . 12)
                               (right-fringe . 12))))

;;; Interface Enhancement:

;; Ensure binding the IBuffer keymap, and customize face.
(use-package ibuffer
  :general
  ("C-x C-b" 'ibuffer)
  ([remap list-buffers] 'ibuffer)
  (:keymaps 'ibuffer-mode-map
            "C-x C-f" 'counsel-find-file)
  :custom
  (ibuffer-filter-group-name-face
  '(:inherit (font-lock-string-face bold))))

;; Show more awesome icons in IBuffer buffer.
(use-package all-the-icons-ibuffer
  :when my-icon
  :straight t
  :blackout t
  :after ibuffer
  :hook (ibuffer-mode-hook . all-the-icons-ibuffer-mode))

;; Show buffer list grouped by `projectile'.
(use-package ibuffer-projectile
  :straight t
  :blackout t
  :after (ibuffer projectile)
  :functions (ibuffer-projectile-set-filter-groups
              buffer-do-sort-by-alphabetic)
  :preface
  (defun my--ibuffer-sort ()
    (ibuffer-projectile-set-filter-groups)
    (unless (eq ibuffer-sorting-mode 'alphabetic)
      (ibuffer-do-sort-by-alphabetic)))
  :hook (ibuffer-mode-hook . my--ibuffer-sort)
  :custom
  (ibuffer-projectile-prefix
   (if my-icon
       (concat
        (all-the-icons-octicon "file-directory"
                               :face ibuffer-filter-group-name-face
                               :v-adjust 0.0
                               :height 1.0)
        " ")
     "Project: ")))

;; `prescient-persist-mode' provides usage statistics to be saved
;; between Emacs sessions.
(use-package prescient
  :straight t
  :blackout t
  :hook (after-init-hook . prescient-persist-mode)
  :custom
  (prescient-save-file (concat my-cache-dir "prescient-save.el")))

;; `ivy' is a generic completion mechanism for Emacs.
(use-package ivy
  :straight counsel
  :blackout (ivy-mode)
  :hook (after-init-hook . ivy-mode)
  :custom
  (ivy-count-format            "%d of %d ")
  (ivy-fixed-height-minibuffer t)
  (ivy-height                  14)
  (ivy-initial-inputs-alist    nil)
  (ivy-re-builders-alist       '((t . ivy--regex-fuzzy)
                                 (t . ivy--regex-plus)
                                 (t . ivy--regex-ignore-order)))
  (ivy-use-virtual-buffers     t)
  (ivy-use-selectable-prompt   t))

;; Use hydra frontend for `ivy-mode'.
(use-package ivy-hydra
  :when (featurep 'ivy)
  :straight t
  :blackout t
  :commands ivy-hydra-read-action
  :custom (ivy-read-action-function #'ivy-hydra-read-action))

;; Ensure use `ivy-prescient-mode' provides sorting and filtering
;; methods for `ivy-mode' by `prescient.el'.
(use-package ivy-prescient
  :when (featurep 'ivy)
  :straight t
  :blackout ivy-prescient-mode
  :hook (ivy-mode-hook . ivy-prescient-mode)
  :custom-face
  (ivy-minibuffer-match-face-1 ((t (:inherit font-lock-doc-face :foreground nil)))))

;; `ivy-rich' provides rich transformers for commands from `ivy' and
;; `counsel'.
(use-package ivy-rich
  :straight t
  :blackout t
  :hook (ivy-mode-hook . ivy-rich-mode)
  :custom
  (ivy-rich-parse-remote-buffer nil))

;; Show more awesome icons for `ivy-mode', mainly use in
;; `counsel-find-file' and `ivy-buffer-switch'.
(use-package all-the-icons-ivy-rich
  :when my-icon
  :straight t
  :blackout t
  :hook (ivy-mode-hook . all-the-icons-ivy-rich-mode)
  :custom
  (all-the-icons-ivy-rich-icon-size 0.8))

;; `ivy-posframe' let `ivy' use `posframe' to show its candidate menu.
(use-package ivy-posframe
  :when my-posframe
  :straight t
  :blackout (ivy-posframe-mode)
  :hook (after-init-hook . ivy-posframe-mode)
  :custom
  (ivy-posframe-parameters '((left-fringe  . 12)
                             (right-fringe . 12))))

;; `counsel' provide versions of common Emacs commands that are
;; customised to make the best use of `ivy'.
(use-package counsel
  :straight t
  :blackout (counsel-mode)
  :hook (after-init-hook . counsel-mode)
  :general
  (:prefix "C-c f"
           "f" '(counsel-find-file :which-key "open file"))
  (:prefix "C-c h"
           "b" 'counsel-descbinds
           "f" 'counsel-describe-function
           "l" 'counsel-load-library
           "s" 'counsel-describe-symbol
           "t" 'counsel-load-theme
           "v" 'counsel-describe-variable
           "F" 'counsel-describe-face))

;; `winner-mode' is a global minor mode, allow undo or redo changes in
;; the window configuration.
(use-package winner
  :blackout (winner)
  :general
  (:prefix "C-c b"
           "p" '(winner-undo :which-key "winner undo")
           "n" '(winner-redo :which-key "winner redo"))
  :hook (after-init-hook . winner-mode)
  :custom
  (winner-dont-bind-my-keys t))

;; `buffer-move' provides swap buffers without typing C-x b on each
;; window.
(use-package buffer-move
  :straight t
  :general
  (:prefix "C-c b"
           "C-n" '(buf-move-down  :which-key "move to down")
           "C-p" '(buf-move-up    :which-key "move to up")
           "C-f" '(buf-move-right :which-key "move to right")
           "C-b" '(buf-move-left  :which-key "move to left"))
  :custom
  (buffer-move-stay-after-swap t))

;; By default, Emacs will show built-in help buffer when press C-h C-h.
;; I hope it display which-key buffer after do same behavoir.
;; The follow config include:
;;   1. bind go back and forward in help buffer.
;;   2. display `help-mode' buffer with popup way.
(use-package help-mode
  :general
  ("C-h C-h" 'nil)
  (help-mode-map
   ("<" 'help-go-back)
   (">" 'help-go-forward))
  :config
  (push '(help-mode :select t :size 0.7 :align 'below :autoclose t)
          shackle-rules))

;; Standard Emacs command `occur' lists all lines of the current
;; buffer that match a regexp that you give it.  The matching lines
;; are listed in buffer *Occur*, and you can click them there to
;; navigate to the corresponding lines in the original buffer.
(use-package replace
  :commands occur
  :config
  (push '(occur-mode :select t :align 'below :autoclose t)
        shackle-rules))

;; `zoom' provides fixed and automatic balanced window layout for
;; Emacs.
(use-package zoom
  :straight t
  :blackout (zoom-mode)
  :hook (after-init-hook . zoom-mode)
  :custom
  (zoom-size '(0.618 . 0.618))
  (zoom-ignored-major-modes '(occur-mode
                              help-mode))
  (zoom-ignored-buffer-names '("*Backtrace*"
                               "*Compile-Log*"
                               "*Error*"
                               "*Help*"
                               "*Warnings*"
                               "*Messages*"
			       "*Flycheck errors*")))

;;; File Manager:

;; `dired' can shows a directory (folder) listing that you can use to
;; perform various operations on files and subdirectories in this
;; directory.
(use-package dired
  :general
  ("C-x d" 'dired)
  (:prefix "C-c f"
           "O" '(dired :which-key "open directory"))
  :custom
  (dired-recursive-deletes 'always)
  (dired-recursive-copies 'always)
  (ls-lisp-use-insert-directory-program t)
  ;; Show directory first.
  (dired-listing-switches "-alh --group-directories-first"))

;; `diredfl' provides colourful dired.
(use-package diredfl
  :straight t
  :hook (dired-mode-hook . diredfl-mode))

;; Use `dired-quick-sort' provides sort in `dired-mode'.
(use-package dired-quick-sort
  :straight t
  :general
  (dired-mode-map "S" 'hydra-dired-quick-sort/body))

;; Use `dired-git-info' to show git info in `dired'.
(use-package dired-git-info
  :straight t
  :general
  (dired-mode-map ")" 'dired-git-info-mode))

;; Use `all-the-icons' in `dired-mode' by `all-the-icons-dired'.
(use-package all-the-icons-dired
  :straight (all-the-icons-dired :type   git
                                 :host   github
                                 :repo   "brsvh/all-the-icons-dired"
                                 :branch "patch")
  :blackout (all-the-icons-dired-mode)
  :hook (dired-mode-hook . all-the-icons-dired-mode)
  :custom-face
  (all-the-icons-dired-dir-face ((t (:height 0.8))))
  (all-the-icons-dired-file-face ((t (:height 0.8)))))

;; `dired-posframe' make `dired' use `posframe', it will preview file and show contents vai `posframe'.
(use-package dired-posframe
  :when my-posframe
  :straight t
  :blackout (dired-posframe-mode)
  :hook (dired-mode-hook . dired-posframe-mode))

;;; Navigation:

;; `windmove' provides move operations between windows.
(use-package windmove
  :general
  (:prefix "C-c w"
           "C-n" 'windmove-up
           "C-p" 'windmove-down
           "C-f" 'windmove-right
           "C-b" 'windmove-left))

;; `winum' provides navigate windows and frames using numbers.
(use-package winum
  :straight t
  :blackout (winum-mode)
  :hook (after-init-hook . winum-mode)
  :config
  (winum-set-keymap-prefix (kbd "C-c w")))

;; `block-nav' provides some commands for navigating through code
;; based on indentation.
(use-package block-nav
  :straight '(block-nav :type git
                        :host github
                        :repo "nixin72/block-nav.el")
  :general
  (:prefix "C-c g"
           "n"   '(block-nav-next-block
                   :which-key "next block")
           "p"   '(block-nav-previous-block
                   :which-key "previous block")
           "C-n" '(block-nav-next-indentation-level
                   :which-key "next indentation")
           "C-p" '(block-nav-previous-indentation-level
                   :which-key "previous indentation")))

;; `avy' provides some commands that can jumping to visible text using
;; a char-based decision tree.
(use-package avy
  :straight t
  :general
  (:prefix "C-c g"
           "c" 'avy-goto-char
           "l" 'avy-goto-line
           "w" 'avy-goto-word-1))

;; `imenu' produces menus for accessing locations in documents,
;; typically in the current buffer.
(use-package imenu
  :general
  (:prefix "C-c g"
           "i" 'imenu))

;; `anzu' is an Emacs port of anzu.vim.  `anzu.el' provides a minor
;; mode which displays current match and total matches information in
;; the mode-line in various search modes.
(use-package anzu
  :straight t
  :blackout (global-anzu-mode anzu-mode)
  :hook (after-init-hook . global-anzu-mode)
  :general
  ([remap query-replace] 'anzu-query-replace)
  ([remap query-replace-regexp] 'anzu-query-replace-regexp)
  (:prefix "C-c e"
           "r" '(:ignore t :which-key "replace")
           "r c" 'anzu-query-replace-at-cursor
           "r t" 'anzu-query-replace-at-cursor-thing))

;; `swiper' is an alternative to isearch that uses ivy to show an
;; overview of all matches.
(use-package swiper
  :straight counsel
  :general
  (:prefix "C-c e"
           "s"   '(:ignore t  :which-key "search")
           "s s" '(swiper     :which-key "search")
           "s S" '(swiper-all :which-key "search in all buffers")))

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

;; Colorize color names in buffers.
;; #000000 #ffffff #00ff00 #ff0000 #0000ff
(use-package rainbow-mode
  :straight t
  :blackout (rainbow-mode)
  :hook (after-init-hook . rainbow-mode))

;; Hightlight current line globally.
(use-package hl-line
  :blackout (global-hl-line-mode hl-line-mode)
  :hook (after-init-hook . global-hl-line-mode))

;; Hightlight TODO and similar keywords in comments and strings
;; TODO FIXME BUG DONE PROG
(use-package hl-todo
  :straight t
  :hook (after-init-hook . global-hl-todo-mode)
  :general
  (:prefix "C-c g"
	   "N" '(hl-todo-next :which-key "next todo")
	   "P" '(hl-todo-previous :which-key "previous todo")))

;; `dimmer' provides a minor mode that indicates which buffer is
;; currently active by dimming the faces in the other buffers.
(use-package dimmer
  :straight t
  :blackout (dimmer-mode)
  :hook (after-init-hook . dimmer-mode)
  :config
  (dimmer-configure-hydra)
  ;; TODO Uncomment this line after activate `magit'
  ;; (dimmer-configure-magit)
  (dimmer-configure-org)
  (dimmer-configure-posframe)
  (dimmer-configure-which-key))

;;; Editing:

;; Mark, edit multiple lines at once.
(use-package multiple-cursors
  :straight t
  :general
  ;; TODO expand more cases.
  (:prefix "C-c e"
           "c"   '(mc/edit-lines :which-key "edit lines")
           "C->" '(mc/mark-next-like-this
                   :which-key "mark next edit")
           "C-<" '(mc/mark-previous-like-this
                   :which-key "mark previous edit")
           "C-c" '(mc/mark-all-like-this "mark all")))

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
  ("C-c p" 'projectile-command-map)
  :config
  (when (featurep 'ivy)
    (setq projectile-completion-system 'ivy)))

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
  :unless my-posframe
  :straight t
  :blackout (company-quickhelp-mode)
  :hook (company-mode-hook . company-quickhelp-mode))

;; `company-posframe' let `company' use child frame as its candidate
;; menu.
(use-package company-posframe
  :when my-posframe
  :straight t
  :blackout (company-posframe-mode)
  :hook (company-mode-hook . company-posframe-mode))

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
                              'right-margin))
  :config
  (push '("*Flycheck errors*" :select t :size 0.5 :align 'below :autoclose t)
        shackle-rules))

;; `flycheck-posframe' display `flycheck' error messages via
;; `posframe'.
(use-package flycheck-posframe
  :when my-posframe
  :straight t
  :blackout (flycheck-posframe-mode)
  :hook (flycheck-mode-hook . flycheck-posframe-mode)
  :config (flycheck-posframe-configure-pretty-defaults))

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

;; `which-key-posframe' use posframe to show which-key popup.
(use-package which-key-posframe
  :when my-posframe
  :straight t
  :blackout (which-key-posframe-mode)
  :hook (which-key-mode-hook . which-key-posframe-mode))

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
  :disabled
  :straight t
  :blackout (mood-line-mode)
  :hook (after-init-hook . mood-line-mode)
  :custom
  (mood-line-show-eol-style            t)
  (mood-line-show-encoding-information t))

;; Awesome and modern modeline: `doom-modeline'.
(use-package doom-modeline
  :straight t
  :blackout t
  :hook (after-init-hook . doom-modeline-mode)
  :custom
  (doom-modeline-height 25)
  (doom-modeline-bar-width 5)
  (doom-modeline-window-width-limit fill-column)
  (doom-modeline-project-detection 'project)
  (doom-modeline-buffer-file-name-style 'auto)
  (doom-modeline-icon my-icon)
  (doom-modeline-major-mode-icon t)
  (doom-modeline-buffer-state-icon t)
  (doom-modeline-buffer-modification-icon t)
  (doom-modeline-unicode-fallback nil)
  (doom-modeline-minor-modes nil)
  (doom-modeline-enable-word-count nil)
  (doom-modeline-continuous-word-count-modes '(gfm-mode
                                               markdown-mode
                                               org-mode))
  (doom-modeline-buffer-encoding t)
  (doom-modeline-indent-info nil)
  (doom-modeline-checker-simple-format t)
  (doom-modeline-number-limit 99)
  (doom-modeline-vcs-max-length 12)
  (doom-modeline-workspace-name t)
  (doom-modeline-display-default-persp-name nil)
  (doom-modeline-persp-icon t)
  (doom-modeline-lsp t)
  (doom-modeline-github nil)
  (doom-modeline-github-interval (* 30 60))
  (doom-modeline-modal-icon t)
  (doom-modeline-mu4e nil)
  (doom-modeline-gnus t)
  (doom-modeline-gnus-timer 2)
  (doom-modeline-gnus-excluded-groups '("dummy.group"))
  (doom-modeline-irc t)
  (doom-modeline-irc-stylize 'identity)
  (doom-modeline-env-version t)
  (doom-modeline-env-load-string "..."))

;; `all-the-icons.el' is a utility package to collect various Icon
;; Fonts and propertize them within Emacs.
(use-package all-the-icons
  :when my-icon
  :straight t
  :functions
  (all-the-icons-icon-for-buffer
   all-the-icons-icon-for-dir
   all-the-icons-icon-for-file
   all-the-icons-icon-for-mode
   all-the-icons-icon-for-url
   all-the-icons-alltheicon
   all-the-icons-faicon
   all-the-icons-fileicon
   all-the-icons-oction
   all-the-icons-wicon))

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
