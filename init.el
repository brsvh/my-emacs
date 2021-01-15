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

;; TODO Lists

;; FIXME `TeX-output-mode' don't follow `zoom' rules and `shackle'
;; rules.

;; PROG prettify TeX mode and LaTeX mode.

;; NEXT use `org-roam' feature.

;; NEXT set valign of `org-mode'.

;; NEXT set a better font of Chinese, include variabled and fixed.

;;; Code:

;; I would like to use early initialization support, it requires Emacs
;; version greater than 27.1.  But I also have machine install Emacs
;; with "feature/native-comp" master, so I can't do Emacs version
;; check.
;; (when (version< emacs-version "27.1")
;;   (error "This requires Emacs 27.1 and above!"))

;;; Prologue:

(require 'xdg)

;; Ensure my directory definitions are bound.
(unless (boundp 'my-dir)
  (defconst my-dir user-emacs-directory
    "The root directory of my Emacs config.")

  (defconst my-data-dir (expand-file-name "emacs/" (xdg-data-home))
    "Directory for my data storage.")

  (defconst my-cache-dir (expand-file-name "emacs/" (xdg-cache-home))
    "Directory for my cache storage.")

  (defconst my-straight-dir (concat my-cache-dir
                                  "straight/repos/straight.el/")
    "Directory for `straight' data storage.")

    ;; Ensure `my-straight-dir' in `load-path'.
  (push my-straight-dir load-path))

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

(require 'straight)

(unless (boundp 'my-straight-initialize-p)
  ;; Download `straight' if it is not installed.
  (unless (file-directory-p my-straight-dir)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))

  ;; Add `my-straight-dir' to `load-path'.
  (push my-straight-dir load-path)

  ;; This assures the byte-compiler that we know what we are doing when
  ;; we reference functions and variables from straight.el below. It
  ;; does not actually do anything at runtime, since the `straight'
  ;; feature has already been provided by loading straight.elc above.
  (require 'straight)

  ;; Customize `straight' behaviors.
  (setq straight-base-dir my-cache-dir
        ;; Use the develop branch of `straight'.
        straight-repository-branch "develop"
        ;; Reset the name of `straight' build directory include
        ;; `emacs-version' infomation to save build history.
        straight-build-dir (format "build-%s/" emacs-version)
        ;; Set clone depth to 1 when use git protocol.
        straight-vc-git-default-clone-depth 1
        ;; This is kind of aggressive but we really don't have a good
        ;; mechanism at present for customizing the default recipe
        ;; repositories anyway. So don't even try to cater to that use
        ;; case.
        straight-recipe-repositories nil
        ;; Use symlinks for building packages.
        straight-use-symlinks t
        ;; Enable integration with `package'.
        straight-enable-package-integration t
        ;; Enable integration with `use-package'.
        straight-enable-use-package-integration t)

  ;; If watchexec and Python are installed, use file watchers to detect
  ;; package modifications. This saves time at startup. Otherwise, use
  ;; the ever-reliable find(1).
  (if (and (executable-find "watchexec")
           (executable-find "python3"))
      (setq straight-check-for-modifications
            '(watch-files find-when-checking))
    (setq straight-check-for-modifications
          '(find-at-startup find-when-checking)))

  ;; In case this is a reinit, and straight.el was already loaded, we
  ;; have to explicitly clear the caches.
  (straight--reset-caches)

  ;; Add Org Emacs lisp Package Archive recipes.
  ;; (straight-use-recipes '(org-elpa
  ;;                         :local-repo nil))

  ;; Add Milkypostman’s Emacs Lisp Package Archive recipes.
  (straight-use-recipes '(melpa
                          :type git
                          :host github
                          :repo "melpa/melpa"
                          :build nil))

  ;; Add GNU ELPA Mirror recipes.
  (straight-use-recipes '(gnu-elpa-mirror
                          :type git :host github
                          :repo "emacs-straight/gnu-elpa-mirror"
                          :build nil))

  ;; Add `el-get' recipes.
  (straight-use-recipes '(el-get
                          :type git
                          :host github
                          :repo "dimitri/el-get"
                          :build nil))

  ;; Add emacsmirror-mirror recipes.
  (straight-use-recipes '(emacsmirror-mirror
                          :type git
                          :host github
                          :repo "emacs-straight/emacsmirror-mirror"
                          :build nil))

  ;; Register `straight' self as package.
  (straight-use-package `(straight
                          :type git
                          :host github
                          :repo ,(format "%s/straight.el"
                                         straight-repository-user)
                          :files ("straight*.el")
                          :branch ,straight-repository-branch))

  ;; Make `straight' check for modifications when
  ;; `straight--modifications' method is `check-on-save'.
  (if (straight--modifications 'check-on-save)
      (straight-live-modifications-mode +1)
    (straight-live-modifications-mode -1))

  ;; When `straight--modifications' method is `check-on-save', use
  ;; `watchexec' watch modifications.
  (when (straight--modifications 'watch-files)
    (straight-watcher-start))

  ;; Emulating symlinks in the software layer when use symlinks for
  ;; building packages.
  (if straight-use-symlinks
      (straight-symlink-emulation-mode nil)
    (straight-symlink-emulation-mode t))

  ;; Enable integration for `package'.
  (if straight-enable-package-integration
      (straight-package-neutering-mode t)
    (straight-package-neutering-mode nil))

  ;; Enable integration for `use-package'.
  (if straight-enable-use-package-integration
      (straight-use-package-mode t)
    (straight-use-package-mode nil)))

;;; Customize:

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

;;; Package manager:

;; Ensure `use-package' is installed and loaded.
(straight-use-package 'use-package)

;; Get rid of reference to free variable warning of symbol name in
;; `use-package' arguments.
(require 'use-package)

;; I use `use-package.el' to organize my Emacs configuration.
;; `use-package' provides `use-package' macro that allows me to
;; isolate package configuration by performance-oriented way.

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

;; The easy way to clean up Emacs mode lighters.
(use-package blackout
  :straight t)

;; Get rid of warning about directly use `blackout' function.
(require 'blackout)

;; Like the advice system, el-patch provides a way to customize the
;; behavior of Emacs Lisp functions that do not provide enough
;; variables and hooks to let you make them do what you want.  The
;; advantage of using el-patch is that you will be notified if the
;; definition of a function you are customizing changes, so that you
;; are aware your customizations might need to be updated.

;; Using the same mechanism, el-patch also provides a way to make
;; lazy-loading packages much more easy, powerful, and robust.
(use-package el-patch
  :straight t)

;; Get rid of warning about directly use `el-patch' function.
(require 'el-patch)

;; More convenient key definitions in Emacs.
(use-package general
  :straight t)

;; Adds the :hydra keyword to the `use-package' macro.
(use-package use-package-hydra
  :straight t)

; Feature `straight-x' from package `straight' provides
;; experimental/unstable extensions to straight.el which are not yet
;; ready for official inclusion.
(use-package straight-x
  :commands (straight-x-fetch-all))

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

;; Emacs will save customization to `custom-file', it is
;; `user-init-file' by default.  I don't want to it littering my
;; `user-init-file', redirect it.
(setq custom-file (concat my-dir "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file nil 'nomessage))

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
  :hook (after-init-hook . super-save-mode)
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
    "Close current popup window via `C-g'.
Also kill process in that window."
    (setq shackle--popup-window-list
          (cl-loop for (window . buffer) in shackle--popup-window-list
                   if (and (window-live-p window)
                           (equal (window-buffer window) buffer))
                   collect (cons window buffer)))
    (when (and (called-interactively-p 'interactive)
               (not (region-active-p)))
      (let (window buffer process)
        (if (one-window-p)
            (progn
              (setq window (selected-window))
              (when (equal (buffer-local-value 'shackle--current-popup-window
                                               (window-buffer window))
                           window)
                (winner-undo)))
          (setq window (caar shackle--popup-window-list))
          (setq buffer (cdar shackle--popup-window-list))
          (setq process (get-buffer-process buffer))
          (when (and (window-live-p window)
                     (equal (window-buffer window) buffer))
            (if process
                (when (process-live-p process)
                  (kill-process process)))
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
                   ("*Finder*"      :size 0.5 :align 'below :autoclose t)
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

;; Replace prettify symbol in `org-mode'.
(use-package prog-mode
  :custom
  (prettify-symbols-alist '(("#+author:"       . "@")
                            ("#+begin_center"  . "⟨")
                            ("#+begin_comment" . "‹")
                            ("#+begin_example" . "‡")
                            ("#+begin_export"  . "«")
                            ("#+begin_quote"   . "※")
                            ("#+begin_src"     . "†")
                            ("#+begin_verse"   . "”")
                            ("#+end_center"    . "⟩")
                            ("#+end_comment"   . "›")
                            ("#+end_example"   . "‡")
                            ("#+end_export"    . "»")
                            ("#+end_quote"     . "※")
                            ("#+end_src"       . "†")
                            ("#+end_verse"     . "”")
                            ("#+name:"         . "≈")
                            ("#+tblfm:"        . "⇨")
                            ("#+title:"        . "❧ ")
                            ("#+property:"     . "☰ "))))

;;; Key-bindings:

(require 'general)
(general-define-key
 :prefix "C-c"
 "a" '(:ignore t :which-key "run")
 "b" '(:ignore t :which-key "buffer")
 "e" '(:ignore t :which-key "edit")
 "f" '(:ignore t :which-key "file")
 "g" '(:ignore t :which-key "goto")
 "h" '(:ignore t :which-key "help")
 "l" '(:ignore t :which-key "language")
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
  (push '(occur-mode :size 0.3 :select t :align right :autoclose t)
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
                              help-mode
                              lsp-ui-imenu-mode
                              vterm-mode
                              eshell-mode
			      TeX-output-mode))
  (zoom-ignored-buffer-names '("*Backtrace*"
                               "*Compile-Log*"
                               "*Error*"
                               "*Help*"
                               "*Warnings*"
                               "*Messages*"
                               "*xref*"
                               "*Flycheck errors*"
			       "*flycheck-posframe-buffer*"
                               "*Kill Ring*"
                               "*cheatsheet*"
                               "*lsp-help*"
			       "LSP Errors View"
			       "*Occur*"))
  (zoom-ignored-buffer-name-regexps '("\\`\\*.*output\\*\\'")))

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

;; A Collection of Ridiculously Useful eXtensions for Emacs.
(use-package crux
  :straight t
  :general
  ("C-k" '(crux-smart-kill-line :which-key "kill line"))
  (:prefix "C-c a"
           "e" '(crux-eval-and-replace :which-key "eval")
           "o" '(crux-open-with :which-key "open with")
           "u" '(crux-view-url  :which-key "view url"))
  (:prefix "C-c b"
           "D" '(crux-delete-file-and-buffer :which-key "delete current file")
           "K" '(crux-kill-other-buffers :which-key "kill other buffers"))
  (:prefix "C-c e"
           "M-u" '(crux-upcase-region :which-key "upcase region")
           "M-l" '(curx-downcase-region :which-key "downcase region")
           "M-c" '(crux-capitalize-region :which-key "capitalize region"))
  (:prefix "C-c f"
           "r" '(crux-recentf-find-file :which-key "open recentf file"))
  (:prefix "C-c f M"
           ""  '(:ignore t :which-key "My Emacs")
           "i" '(crux-find-user-init-file :which-key "open init file")
           "c" '(crux-find-user-custom-file :which-key "open custom file")))

;; Edit as sudoer.
(use-package sudo-edit
  :straight t
  :general
  (:prefix "C-c f"
           "s" '(sudo-edit :which-key "sudo edit")
           "S" '(sudo-edit-find-file :which-key "sudo open")))

;; Mark, edit multiple lines at once.
;;
;; * Mark one more occurrence
;;
;; | mc/mark-next-like-this            | Adds a cursor and region at the next part of the buffer       |
;; |                                   | forwards that matches the current region.                     |
;; | mc/mark-next-word-like-this       | Like `mc/mark-next-like-this` but only for whole words.       |
;; | mc/mark-next-symbol-like-this     | Like `mc/mark-next-like-this` but only for whole symbols.     |
;; | mc/mark-previous-like-this        | Adds a cursor and region at the next part of the buffer       |
;; |                                   | backwards that matches the current region.                    |
;; | mc/mark-previous-word-like-this   | Like `mc/mark-previous-like-this` but only for whole words.   |
;; | mc/mark-previous-symbol-like-this | Like `mc/mark-previous-like-this` but only for whole symbols. |
;; | mc/mark-more-like-this-extended   | Use arrow keys to quickly mark/skip next/previous occurances. |
;; | mc/add-cursor-on-click            | Bind to a mouse event to add cursors by clicking.             |
;; |                                   | See tips-section.                                             |
;;
;; * Mark many occurrences
;;
;; | mc/mark-all-like-this                  | Marks all parts of the buffer that matches the current region.        |
;; | mc/mark-all-words-like-this            | Like `mc/mark-all-like-this` but only for whole words.                |
;; | mc/mark-all-symbols-like-this          | Like `mc/mark-all-like-this` but only for whole symbols.              |
;; | mc/mark-all-in-region                  | Prompts for a string to match in the region, adding cursors           |
;; |                                        | to all of them.                                                       |
;; | mc/mark-all-like-this-in-defun         | Marks all parts of the current defun that matches the current region. |
;; | mc/mark-all-words-like-this-in-defun   | Like `mc/mark-all-like-this-in-defun` but only for whole words.       |
;; | mc/mark-all-symbols-like-this-in-defun | Like `mc/mark-all-like-this-in-defun` but only for whole symbols.     |
;; | mc/mark-all-like-this-dwim             | Tries to be smart about marking everything you want. Can be           |
;; |                                        | pressed multiple times.                                               |
(use-package multiple-cursors
  :straight t
  :general
  ;; TODO expand more cases.
  (:prefix "C-c e m"
           ""    '(:ignore t :which-key "multi-edit")
           "a"   '(mc/mark-all-like-this
                   :which-key "mark all")
           "p"   '(mc/mark-previous-like-this
                   :which-key "mark previous")
           "n"   '(mc/mark-next-like-this
                   :which-key "mark next")
           "P"   '(mc/unmark-previous-like-this
                   :which-key "unmark previous")
           "N"   '(mc/unmark-next-like-this
                   :which-key "unmark next")
           "["   '(mc/cycle-backward
                   :which-key "cycle backward")
           "]"   '(mc/cycle-forward
                   :which-key "cycle forward")
           "m"   '(mc/mark-more-like-this-extended
                   :which-key "mark extended")
           "h"   '(mc-hide-unmatched-lines-mode
                   :which-key "hide unmatch")
           "\\"  '(mc/vertical-align-with-space
                   :which-key "alight space")
           "#"   '(mc/insert-numbers
                   :which-key "insert number")
           "^"   '(mc/edit-beginnings-of-lines
                   :which-key "edit begin")
           "$"   '(mc/edit-ends-of-lines
                   :which-key "edit end")))

;; `undo-fu' is yet another `undo-tree'.
(use-package undo-fu
  :straight t
  :general
  ("C-/" '(undo-fu-only-undo :which-key "undo"))
  (:prefix "C-c e"
           "u" '(undo-fu-only-undo :which-key "undo")
           "R" '(undo-fu-only-redo :which-key "redo")))

;; Kill-ring:

;; `browse-kill-ring' provides insert item from kill-ring.
(use-package browse-kill-ring
  :straight t
  :general
  ("C-c e k" '(browse-kill-ring :which-key "kill ring"))
  :config
  (push '("*Kill Ring*" :select t :size 0.4 :align 'below :autoclose t)
        shackle-rules))

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
  (:prefix "C-c p"
           "4" '(:ignore t :which-key "find other")
           "5" '(:ignore t :which-key "find other")
           "s" '(:ignore t :which-key "grep")
           "x" '(:ignore t :which-key "shell"))
  :config
  (when (featurep 'ivy)
    (setq projectile-completion-system 'ivy)))

;;; Programming:

;; Wakatime service provides statistics and analysis of programming.
(use-package wakatime-mode
  :straight t
  :blackout t
  :hook (after-init-hook . global-wakatime-mode))

;; `xref' provides a somewhat generic infrastructure for cross
;; referencing commands, in particular "find-definition".
(use-package xref
  :defer t
  :config
  (push '("*xref*" :size 0.5 :align 'below :autoclose t)
        shackle-rules))

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

;; `indent-guide' use to show vertical lines to guide indentation.
(use-package indent-guide
  :disabled
  :straight t
  :blackout (indent-guide-mode indent-guide-global-mode)
  ;; :hook (prog-mode-hook . indent-guide-mode))
  :hook (after-init-hook . indent-guide-global-mode)
  :custom (indent-guide-global-mode '(not vterm-mode org-mode)))

;; `highlight-indent-guides' is another indent guide.
(use-package highlight-indent-guides
  :straight t
  :blackout highlight-indent-guides-mode
  :hook (prog-mode-hook . highlight-indent-guides-mode)
  :custom
  (highlight-indent-guides-method 'column))

;; `lsp-mode' provides language server protocol support for Emacs.
(use-package lsp-mode
  :straight t
  :blackout t
  :defer t
  :commands lsp
  :hook (lsp-mode-hook . lsp-enable-which-key-integration)
  :custom
  (lsp-keymap-prefix (kbd "C-c l"))
  :config
  (push '("*lsp-help*" :size 0.5 :align below :autoclose t)
        shackle-rules)
  (push '("LSP Errors View" :align below :autoclose t)
        shackle-rules))

;; Hight level UI modules of `lsp'.
(use-package lsp-ui
  :when (featurep 'lsp-mode)
  :straight t
  :blackout t
  :defer t
  :general
  ([remap xref-find-definitions] 'lsp-ui-peek-find-definitions)
  ([remap xref-find-references]  'lsp-ui-peek-find-references))

;; Auto-format source code in many languages with one command.
(use-package format-all
  :straight t
  :blackout (format-all-mode)
  :commands (format-all-buffer)
  :general
  (:prefix "C-c l"
           "f" '(format-all-buffer :which-key "format buffer")))

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

;;; Debugging:

;; Just like `lsp-mode' provides editing features through the
;; editor-independent language server protocol, its sister package
;; `dap-mode' provides debugging features through the
;; editor-independent debug adapter protocol and language-specific
;; debugging servers called debug adapters. front-end of the sister
;; debug adapter protocol. It is again client server like language
;; sever protocol. `dap-mode' provides all of the traditional debugger
;; features - breakpoints(conditions, hit count, etc), threads,
;; locals.
(use-package dap-mode
  :straight t
  :blackout t
  :defer t)

;; Documentation:

;; `eldoc' is built-in package to shows function arguments / variable
;; doc in minibuffer when coding.
(use-package eldoc
  :blackout t
  :hook (prog-mode-hook . eldoc-mode))

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
        shackle-rules)
  (push '("*flycheck-posframe-buffer*" :select t :size 0.5 :align 'below :autoclose t)
        shackle-rules))

;; `flycheck-posframe' display `flycheck' error messages via
;; `posframe'.
(use-package flycheck-posframe
  :when my-posframe
  :straight t
  :blackout (flycheck-posframe-mode)
  :hook (flycheck-mode-hook . flycheck-posframe-mode)
  :config (flycheck-posframe-configure-pretty-defaults))

;;; Programming Language:

;;; C/C++:

;; `cc-mode' provides support of C and other languages with similar syntax.
(use-package cc-mode
  :hook
  (c-mode-hook . lsp)
  (c++-mode-hook . lsp))

;;; Emacs Lisp:
(use-package ielm
  :commands ielm
  :config
  (push '("*ielm*" :select t :align 'below :autoclose t) shackle-rules))

;;; Keys Cheat Sheet:

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

;; `key-assist' is a tool for creating my own Emacs cheatsheet.
(use-package key-assist
  :straight t
  :general
  ("C-c h c" '(key-assist :which-key "cheatsheet")))

;;; Note:

;;; Org:

;; Write notes, GTD, authoring, publish and wash dishes.
(use-package org
  :straight (org :host github :repo "emacs-straight/org-mode" :local-repo "org")
  :hook (org-mode-hook . visual-line-mode)
  :custom-face
  (org-document-title        ((t (:height 2.25
                                  :underline nil))))
  (org-document-info         ((t (:height 1.375))))
  (org-document-info-keyword ((t (:height 1.625))))

  (org-level-1 ((t (:height 2.000))))
  (org-level-2 ((t (:height 1.875))))
  (org-level-3 ((t (:height 1.750))))
  (org-level-4 ((t (:height 1.625))))
  (org-level-5 ((t (:height 1.500))))
  (org-level-6 ((t (:height 1.375))))
  (org-level-7 ((t (:height 1.250))))
  (org-level-8 ((t (:height 1.125))))
  :custom
  (org-pretty-entities t)
  (org-ellipsis "↪")
  (org-hide-emphasis-markers t)
  (org-fontify-whole-heading-line t)
  (org-fontify-done-headline t)
  (org-fontify-quote-and-verse-blocks t)
  (org-src-fontify-natively t)
  (org-todo-keywords '((sequence "TODO" "NEXT" "PROG" "WAIT" "FAIL" "DONE")))
  (org-todo-keyword-faces '(("TODO" :foreground "salmon" :weight bold)
                            ("NEXT" :foreground "deep sky blue" :weight bold)
                            ("PROG" :foreground "magenta" :weight bold)
                            ("WAIT" :foreground "peru" :weight bold)
                            ("FAIL" :foreground "red" :weight bold)
                            ("DONE" :foreground "forest green" :weight bold))))

;; `org-variable-pitch' provides variable-pitch support for `org-mode'.
(use-package org-variable-pitch
  :straight t
  :blackout (org-variable-pitch-minor-mode)
  :hook
  (org-mode-hook . org-variable-pitch-minor-mode)
  (org-variable-pitch-minor-mode-hook . prettify-symbols-mode)
  :custom-face
  (variable-pitch ((t (:font "Cardo"))))
  :custom
  (org-variable-pitch-fixed-font "Consolas"))

;; `org-superstart-mode' provides prettify headings and plain lists in
;; Org mode.
(use-package org-superstar
  :straight t
  :when my-icon
  :blackout t
  :hook (org-mode-hook . org-superstar-mode)
  :custom
  (org-superstar-leading-bullet ?\s)
  (org-superstar-leading-fallback ?\s)
  (org-hide-leading-stars nil)
  (org-superstar-headline-bullets-list '("§"))
  (org-superstar-item-bullet-alist '((?* . ?○)
                                     (?+ . ?▸)
                                     (?- . ?●))))

;;; Version Control:

;;; Integration:

;;; Console:

;; `eshell' is Emacs built-in package that provides shell evaluate.
(use-package eshell
  :preface
  (defun quarter-window-vertically ()
    "create a new window a quarter size of the current window"
    (split-window-vertically)
    (other-window 1)
    (split-window-vertically)
    (other-window -1)
    (delete-window))
  
  (defun open-mini-eshell ()
    "open a mini-eshell in a small window at the bottom of the current window"
    (interactive)
    (quarter-window-vertically)
    (other-window 1)
    (eshell))
  :general
  (:prefix "C-c a"
           "s" '(open-mini-eshell :which-key "eshell")
           "S" '(eshell :which-key "eshell buffer"))
  :config
  (push '("*eshell*" :select t :align 'below :autoclose t) shackle-rules))

;; A fully-fledged terminal emulator inside Emacs based on libvterm.
(use-package vterm
  :straight t
  :preface
  ;; HACK kill `shackle' window when vterm process exit.
  (defun my-vterm--sentinel (process event)
    "Sentinel of vterm PROCESS.
Argument EVENT process event."
    (let* ((buf (process-buffer process))
           (window (get-buffer-window buf)))
      (run-hook-with-args 'vterm-exit-functions
                          (if (buffer-live-p buf) buf nil)
                          event)
      (if (and vterm-kill-buffer-on-exit (buffer-live-p buf))
          (progn
            (when window
              (delete-window window))
            (kill-buffer buf)))))
  :commands (vterm vterm-other-window)
  :general
  ("C-c a t" '(vterm-other-window :which-key "vterm"))
  ("C-c a T" '(vterm :which-key "vterm buffer"))
  :custom
  (vterm-shell "zsh")
  (vterm-kill-buffer-on-exit t)
  :config
  (advice-add #'vterm--sentinel :override #'my-vterm--sentinel)
  (push '(vterm-mode :select t :size 0.3 :align 'below :autoclose t)
	shackle-rules))

;;; LaTex:

;; AUCTeX is THE TeX extension for Emacs.  Be careful with
;; configuration because it overrides the built-in tex package.
(use-package auctex
  :straight t
  :mode ("\\.tex\\'" . TeX-latex-mode)
  :hook
  (LaTeX-mode-hook . visual-line-mode)
  (LaTeX-mode-hook . turn-on-reftex)
  (LaTeX-mode-hook . lsp)
  :custom
  (TeX-master nil)
  (TeX-auto-save t)
  (TeX-parse-self t)
  (reftex-plug-into-AUCTeX t)
  (TeX-command-default "XeLaTeX")
  (TeX-command-list
   '(("XeLaTeX"
      "%`xelatex%(mode)%' %t"
      TeX-run-command nil t
      :help "Run XeLaTeX")
     ("Tectonic" "tectonic %t"
      TeX-run-command nil t
      :help "Run tectonic")
     ("LatexMk"
      "latexmk %(-PDF)%S%(mode) %(file-line-error) %(extraopts) %t"
      TeX-run-latexmk nil
      (plain-tex-mode
       latex-mode
       doctex-mode)
      :help "Run LatexMk")
     ("TeX"
      "%(PDF)%(tex) %(file-line-error) %`%(extraopts) %S%(PDFout)%(mode)%' %t"
      TeX-run-TeX nil
      (plain-tex-mode
       texinfo-mode
       ams-tex-mode)
      :help "Run plain TeX")
     ("LaTeX"
      "%`%l%(mode)%' %T"
      TeX-run-TeX nil
      (latex-mode
       doctex-mode)
      :help "Run LaTeX")
     ("Makeinfo"
      "makeinfo %(extraopts) %t"
      TeX-run-compile nil
      (texinfo-mode)
      :help "Run Makeinfo with Info output")
     ("Makeinfo HTML"
      "makeinfo %(extraopts) --html %t"
      TeX-run-compile nil
      (texinfo-mode)
      :help "Run Makeinfo with HTML output")
     ("AmSTeX"
      "amstex %(PDFout) %`%(extraopts) %S%(mode)%' %t"
      TeX-run-TeX nil
      (ams-tex-mode)
      :help "Run AMSTeX")
     ("ConTeXt"
      "%(cntxcom) --once --texutil %(extraopts) %(execopts)%t"
      TeX-run-TeX nil
      (context-mode)
      :help "Run ConTeXt once")
     ("ConTeXt Full"
      "%(cntxcom) %(extraopts) %(execopts)%t"
      TeX-run-TeX nil
      (context-mode)
      :help "Run ConTeXt until completion")
     ("BibTeX"
      "bibtex %s"
      TeX-run-BibTeX nil
      (plain-tex-mode
       latex-mode
       doctex-mode
       context-mode
       texinfo-mode
       ams-tex-mode)
      :help "Run BibTeX")
     ("Biber"
      "biber %s"
      TeX-run-Biber nil
      (plain-tex-mode
       latex-mode
       doctex-mode
       texinfo-mode
       ams-tex-mode)
      :help "Run Biber")
     ("View"
      "%V"
      TeX-run-discard-or-function t t
      :help "Run Viewer")
     ("Print"
      "%p"
      TeX-run-command t t
      :help "Print the file")
     ("Queue"
      "%q"
      TeX-run-background nil t
      :help "View the printer queue"
      :visible TeX-queue-command)
     ("File"
      "%(o?)dvips %d -o %f "
      TeX-run-dvips t
      (plain-tex-mode
       latex-mode
       doctex-mode
       texinfo-mode
       ams-tex-mode)
      :help "Generate PostScript file")
     ("Dvips"
      "%(o?)dvips %d -o %f "
      TeX-run-dvips nil
      (plain-tex-mode
       latex-mode
       doctex-mode
       texinfo-mode
       ams-tex-mode)
      :help "Convert DVI file to PostScript")
     ("Dvipdfmx"
      "dvipdfmx %d"
      TeX-run-dvipdfmx nil
      (plain-tex-mode
       latex-mode
       doctex-mode
       texinfo-mode
       ams-tex-mode)
      :help "Convert DVI file to PDF with dvipdfmx")
     ("Ps2pdf"
      "ps2pdf %f"
      TeX-run-ps2pdf nil
      (plain-tex-mode
       latex-mode
       doctex-mode
       texinfo-mode
       ams-tex-mode)
      :help "Convert PostScript file to PDF")
     ("Glossaries"
      "makeglossaries %s"
      TeX-run-command nil
      (plain-tex-mode
       latex-mode
       doctex-mode
       texinfo-mode
       ams-tex-mode)
      :help "Run makeglossaries to create glossary file")
     ("Index"
      "makeindex %s"
      TeX-run-index nil
      (plain-tex-mode
       latex-mode
       doctex-mode
       texinfo-mode
       ams-tex-mode)
      :help "Run makeindex to create index file")
     ("upMendex"
      "upmendex %s"
      TeX-run-index t
      (plain-tex-mode
       latex-mode
       doctex-mode
       texinfo-mode
       ams-tex-mode)
      :help "Run upmendex to create index file")
     ("Xindy"
      "texindy %s"
      TeX-run-command nil
      (plain-tex-mode
       latex-mode
       doctex-mode
       texinfo-mode
       ams-tex-mode)
      :help "Run xindy to create index file")
     ("Check"
      "lacheck %s"
      TeX-run-compile nil
      (latex-mode)
      :help "Check LaTeX file for correctness")
     ("ChkTeX"
      "chktex -v6 %s"
      TeX-run-compile nil
      (latex-mode)
      :help "Check LaTeX file for common mistakes")
     ("Spell"
      "(TeX-ispell-document \"\")"
      TeX-run-function nil t
      :help "Spell-check the document")
     ("Clean"
      "TeX-clean"
      TeX-run-function nil t
      :help "Delete generated intermediate files")
     ("Clean All"
      "(TeX-clean t)"
      TeX-run-function nil t
      :help "Delete generated intermediate and output files")
     ("Other"
      ""
      TeX-run-command t t
      :help "Run an arbitrary command")))
  (TeX-view-program-selection '((output-pdf "pdf-tools")))
  (TeX-view-program-list '(("pdf-tools" "TeX-pdf-tools-sync-view")))
  (TeX-source-correlate-mode t)
  (TeX-source-correlate-start-server t)
  :config
  (push '("\\`\\*.*output\\*\\'" :regexp t :align 'below :autoclose t)
        shackle-rules))

;; Fast input methods to enter LaTeX environments and math with GNU
;; Emacs.
(use-package cdlatex
  :straight t
  :blackout t
  :hook
  (LaTeX-mode-hook . cdlatex-mode)
  (org-mode-hook . org-cdlatex-mode))

;; `auctex-latexmk' adds LatexMk support to AUCTeX.
(use-package auctex-latexmk
  :straight t
  :blackout t
  :after latex
  :custom
  (auctex-latexmk-inherit-TeX-PDF-mode t)
  :config
  (auctex-latexmk-setup))

;; `company-auctex' provides auto-completion for `AucTeX'.
(use-package company-auctex
  :straight t
  :blackout t
  :hook (TeX-latex-mode-hook . company-auctex-bmode)
  :config
  (company-auctex-init))

;;; PDF:

;; `pdf-tools' provides major mode for rendering PDF files, much
;; better than DocView, and has much richer set of features.
(use-package pdf-tools
  :straight t
  :mode ("\\.pdf$" . pdf-view-mode)
  :hook (pdf-view-mode-hook . auto-revert-mode)
  :config
  (pdf-tools-install)
  (push '(pdf-view-mode :align right :select nil) shackle-rules))

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

(provide 'init)
;;; init.el ends here
