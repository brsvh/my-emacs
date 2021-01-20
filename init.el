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

;; To see the outline of this file, run M-x occur with the following
;; query:
;;   ^;;;;* \|^(use-package \|^(def*

;;; Code:

;; Emacs supported early initialization after Emacs 26, it provides
;; huge performance improvements though migrate graphics rendering
;; related customization to it.  I have done a lot of optimizations in
;; it, so the first thing is to make sure it is loaded.  This is to
;; resolve that in some cases the early initialization is not
;; preloaded, such as during use `esup'.

;; Another important reason is that I do `straight' initialization in
;; early initialization.  It is used instead of `package' to manage
;; all third-party packages.  To avoid the annoying warnings when
;; `flycheck' checks the correctness of this file, it also needs to do
;; this step.
(require
 'early-init (expand-file-name "early-init" user-emacs-directory))

;;; Prologue:

;; This section provides a great pre-configured environment for
;; further configuration.  This environment use `company' complete
;; completion of the candidates, use `flycheck' check for the Emacs
;; Lisp code correctness, use `straight' manage third-party packages
;; and use `use-package' manage configuration.  And also support
;; `paredit' method too.  The point of this section is to ensure the
;; semantic correctness of this configuration.

(eval-and-compile
  (require 'cl-lib))

;; I use `use-package.el' to organize my Emacs configuration.
;; `use-package' provides `use-package' macro that allows me to
;; isolate package configuration by performance-oriented way.
(eval-when-compile
  ;; This wrap with `eval-when-compile' is not necessary, just for get
  ;; rid of reference to free variable warning of symbol name in
  ;; `use-package' arguments.
  (straight-use-package 'use-package))

;; Customize the default behaviors of `use-package'.
(use-package use-package
  :custom
  ;; According to the `use-package' documentation, `use-package'
  ;; determines whether deferred loading is required based on some
  ;; keywords, such as ":bind", ":hook", and ":mode", etc.  So most of
  ;; the time I do not need to specify ":defer" keyword, and only
  ;; explicitly use it when I really need it.
  (use-package-always-defer nil)

  ;; Another different thing is use the full name of hooks.  By
  ;; default, when using ":hook" omit the "-hook" suffix if you
  ;; specify the hook explicitly, as this is appended by default.  For
  ;; example the following code will not work as it attempts to add to
  ;; the `prog-mode-hook-hook' which does not exist.  This
  ;; simplification can lead to bad habits, so use the real name of
  ;; hooks instead of a shorter version: `after-init' ==>
  ;; `after-init-hook'.
  (use-package-hook-name-suffix nil)

  ;; Some other useful settings.
  (use-package-compute-statistics   nil)
  (use-package-enable-imenu-support t)
  (use-package-expand-minimally     nil))

;; Emacs will show major and minor modes in the mode line by default,
;; it's too distracting.  `blackout' is a package that allows you to
;; hide or customize the display of major and minor modes in the mode
;; line.  I use `use-package' keywords named ":blackout" to do
;; customization, ensure it is normally configured.
(eval-when-compile
  ;; This wrap with `eval-when-compile' is not necessary, it only for
  ;; get rid of `flycheck' annoying message.
  (straight-use-package 'blackout))

(use-package blackout
  :straight t)

;; Like the advice system, el-patch provides a way to customize the
;; behavior of Emacs Lisp functions that do not provide enough
;; variables and hooks to let you make them do what you want.  The
;; advantage of using el-patch is that you will be notified if the
;; definition of a function you are customizing changes, so that you
;; are aware your customizations might need to be updated.

;; Using the same mechanism, el-patch also provides a way to make
;; lazy-loading packages much more easy, powerful, and robust.
(eval-when-compile
  ;; This wrap with `eval-when-compile' is not necessary, just for get
  ;; rid of the warning about directly use `el-patch' function.
  (straight-use-package 'el-patch)
  (require 'el-patch))

(use-package el-patch
  :straight t
  ;; Patching `defvar', `defconst', and `defcustom' forms will not
  ;; affect the value of the variable, if it has already been
  ;; defined. Thus, they are only useful for lazy-loading by
  ;; default. Set this variable to override this behavior and force
  ;; the patches to reset the value of the variable, even if it is
  ;; already defined.
  ;; ref: https://github.com/raxod502/el-patch#defining-patches
  :custom (el-patch-use-aggressive-defvar t))

;; I have deferred garbage collection further back in the startup
;; process.  Revert to garbage collection with `gcmh' after Emacs
;; startup.  `gcmh' is a package provides magic hack of garbage
;; collector.  Configured it at the beginning because of the value of
;; `gc-cons-threshold' have set to `most-positive-fixnum', it is not
;; appropriate, so resolve it earliest possible.
(use-package gcmh
  :straight t
  :hook (after-init-hook . gcmh-mode)
  :custom ((gcmh-verbose             t)
           (gcmh-lows-cons-threshold #x800000)
           (gcmh-high-cons-threshold most-positive-fixnum)
           (gcmh-idle-delay          30))
  :blackout t)

;; `paredit' is a minor mode for performing structured editing of
;; S-expression data.  For simpler write Emacs Lisp code and keep
;; parentheses balanced, I use it not just for Emacs Lisp. It brings
;; structural editing to lisps, maintaining the syntactical
;; correctness of my code.
(use-package paredit
  :straight (paredit
             :type git
             :repo "https://mumble.net/~campbell/git/paredit.git")
  :hook ((eval-expression-minibuffer-setup-hook
          emacs-lisp-mode-hook
          ielm-mode-hook
          lisp-mode-hook
          lisp-interaction-mode-hook) . paredit-mode)
  :blackout t)

;; A important tool can enhanced productivity - Completion System.
;; `company' is a text completion framework for Emacs, it provides
;; complete anything.
(use-package company
  :straight t
  :hook (after-init-hook . global-company-mode)
  :commands (company-cancel)
  :bind ((:map company-active-map
          ("C-p"    . company-select-previous)
          ("C-n"    . company-select-next)
          ("<up>"   . company-select-previous)
          ("<down>" . company-select-next)
          ("<tab>"  . company-complete-common-or-cycle))
         (:map company-search-map
          ("C-p"   . company-select-previous)
          ("C-n"   . company-select-next)
          ("<up>"   . company-select-previous)
          ("<down>" . company-select-next)))
  :functions (company-dabbrev-ignore-case
              company-dabbrev-downcase)
  :custom ((company-backends '(company-capf
                               (company-dabbrev-code
                                company-keywords
                                company-files)
                               company-dabbrev))
           (company-dabbrev-downcase nil)
           (company-dabbrev-ignore-case nil)
           (company-echo-delay (if (display-graphic-p) nil 0))
           (company-global-modes '(not erc-mode
                                       message-mode
                                       help-mode
                                       gud-mode
                                       eshell-mode
                                       shell-mode))
           (company-idle-delay 0)
           (company-minimum-prefix-length 2)
           (company-require-match nil)
           (company-tooltip-align-annotations t)
           (company-tooltip-limit 12))
  :blackout t)

;; Customize Emacs is written Emacs Lisp code.  As with any other
;; programming language, I need a syntax checker to ensure the code is
;; written syntactically correctly.
;;
;; The default `flycheck-command-map' bind to "C-c !".
(use-package flycheck
  :straight t
  :hook (after-init-hook . global-flycheck-mode)
  :custom ((flycheck-display-errors-delay 0)
           (flycheck-emacs-lisp-load-path 'inherit)
           (flycheck-global-modes '(not text-mode
                                        outline-mode
                                        fundamental-mode
                                        lisp-interaction-mode
                                        org-mode
                                        diff-mode
                                        shell-mode
                                        eshell-mode
                                        term-mode
                                        vterm-mode)))
  :blackout t)

;; Although Emacs have built-in profiler, using it monitor the whole
;; stratup process requires cumbersome configuration.  So I use `esup'
;; that provides simple, straightforward and intuitive monitoring and
;; analysis.  After adding configuration incrementally, use the `esup'
;; command to ensure that performance is not affected.
(use-package esup
  :straight t
  :commands esup
  :custom (esup-depth 0))

;;; Customize:

;; This section defines some of the customize options provided by this
;; configuration for more flexible management.

;; Emacs provides a customization system, I use it manage my
;; customization options I defined by category.
(defgroup my nil
  "My customization options group."
  :group 'emacs)

;; In selecting the root prefix of `my-keymap', one thing to considers
;; is to try to avoid conflicts with the common key binding prefixes
;; of the other packages.  Inspired by `radian', I select the "M-p"
;; key, make it declaration to customization option for modify at any
;; time.
(defcustom my-keymap-prefix "M-p"
  "The prefix key of `my-keymap'."
  :group 'my
  :type 'string)

;; Emacs has a modern killer feature named `posframe' that supported
;; create child-frame connected to its root window's buffer.  It seems
;; good if keep this feature always open, but I only enable it when I
;; want.
(defcustom my-posframe (display-graphic-p)
  "Enable or disable packages who use `posframe'."
  :group 'my
  :type 'boolean)

;; Emacs has some third-party packages that provide wonderful icons
;; support, but they do not work in the terminal.  And I also need
;; have a switch to disable it when I want.
(defcustom my-icons (display-graphic-p)
  "Enable or disable icons."
  :group 'my
  :type 'boolean)

;; I use `shackle' to manage the rules of popup buffers, while
;; presetting some rules.
(defcustom my-buffer-rules
  '(("*Backtrace*"
     :size 0.7 :autoclose t :select t :align below)
    ("*compilation*"
     :size 0.3 :autoclose t :select nil :align below)
    ("*Compile-Log*"
     :size 0.3 :autoclose t :select t :align below)
    ("*Finder*"
     :size 0.3 :autoclose t :select t :align below)
    ("*Flycheck errors*"
     :size 0.3 :autoclose t :select t :align below)
    ("*flycheck-posframe-buffer*"
     :size 0.3 :autoclose t :select t :align below)
    ("*Error*"
     :size 0.3 :autoclose t :select t :align below)
    ("*emacs-lisp-macroexpand*"
     :size 0.3 :autoclose t :select t :align below)
    ("*esup*"
     :same t)
    ("*Help*"
     :size 0.3 :autoclose t :select nil :align below)
    ("*Kill Ring*"
     :size 0.4 :autoclose t :select nil :align below)
    ("*LanguageTool Errors*"
     :size 0.2 :select t :align below :autoclose t)
    ("LSP Errors View"
     :size 0.4 :select t :align below :autoclose t)
    ("*lsp-help*"
     :size 0.4 :select nil :align below :autoclose t)
    ("*Messages*"
     :size 0.3 :autoclose t :select t :align below)
    ("*Occur*"
     :size 0.3 :select t :align right)
    ("*straight*"
     :same t)
    ("*Warnings*"
     :size 0.3 :autoclose t :select t :align below)
    ("*xref*"
     :size 0.3 :select t :align right)
    (help-mode
     :size 0.3 :autoclose t :select nil :align below)
    (pdf-view-mode
     :size 0.4 :align right :select nil)
    ("\\`\\*.*output\\*\\'" :regexp t :align 'below :autoclose t))
  "My popup window rules."
  :group 'my
  :type '(alist
          :key-type (choice
                     :tag "Condition"
                     (symbol :tag "Major mode")
                     (string :tag "Buffer name")
                     (repeat (choice (symbol :tag "Major mode")
                                     (string :tag "Buffer name")))
                     (list :tag "Custom function"
                           (const :tag "Custom" :custom) function))
          :value-type (plist :options
                             (((const :tag "Regexp" :regexp) boolean)
                              ((const :tag "Select" :select) boolean)
                              ((const :tag "Custom" :custom) function)
                              ((const :tag "Inhibit window quit"
                                      :inhibit-window-quit) boolean)
                              ((const :tag "Ignore" :ignore) boolean)
                              ((const :tag "Other" :other) boolean)
                              ((const :tag "Same" :same) boolean)
                              ((const :tag "Popup" :popup) boolean)
                              ((const :tag "Align" :align)
                               (choice :tag "Alignment" :value t
                                       (const :tag "Default" t)
                                       (const :tag "Above" 'above)
                                       (const :tag "Below" 'below)
                                       (const :tag "Left" 'left)
                                       (const :tag "Right" 'right)
                                       (function :tag "Function")))
                              ((const :tag "Size" :size) number)
                              ((const :tag "Frame" :frame) boolean)
                              ((const :tag "Auto Close"
                                      :autoclose) boolean)))))

(defcustom my-lsp t
  "Use LSP or not."
  :group 'my
  :type 'boolean)

(defcustom my-lsp-tex (and my-lsp
			   (or (executable-find "texlab")
			       (executable-find "digestif")))
  "TeX LSP support status."
  :group 'my
  :type 'boolean)

;; Emacs will save customization to `custom-file', it is
;; `user-init-file' by default.  I don't want to it littering my
;; `user-init-file', redirect output to it.
(defconst my-custom-file (concat my-dir "custom.el")
  "The path of my custom file.")

(setq custom-file my-custom-file)

;; And I also have a series of customizations needs can not do in
;; `user-init-file'.
(use-package cus-edit
  :when (file-exists-p my-custom-file)
  :init (load custom-file nil 'nomessage))

;;; Key Binding:

;; Emacs has a powerful key/map binding system.  According to the
;; article write by Mickey Petersen, altering the key bindings in
;; Emacs should not, on the face of it, be a difficult task.  But
;; there’s a reason why the Emacs manual has dedicated 30-odd pages to
;; describing, in great detail, all the subtleties and nuances of how
;; to bind keys.  If I completely follow the Emacs manual and use the
;; low-level functions described in it to configure the key binding,
;; the configuration will look very messy.

;; Fortunately the configuration management tool that I use have
;; built-in tool to help I do key binding, this tool named `bind-key'
;; and `general'. It has been integrated in `use-package' macro, the
;; only thing I need to do is use ":bind", ":general" and
;; ":bind-keymap" keywords.

;; For convenience to customize root key of my bindings at any time, I
;; defined a keymap and change it binding behavior when need.
(defvar my-keymap (make-sparse-keymap)
  "Keymap for my commands that should be put under a prefix.
This keymap is bound under \\[my-keymap].")

;; Because of prefix key of `my-keymap' can be customize, ensure bind
;; it after load `custom-file'.
(bind-key my-keymap-prefix my-keymap)

;; I can bind thousands of special behavior with key or keymaps by use
;; Emacs key-binding system.  But sometimes I will forget my settings,
;; `which-key' support display available keybindings in popup buffer.
(use-package which-key
  :straight t
  :hook (after-init-hook . which-key-mode)
  :custom ((which-key-show-transient-maps  t)
           (which-key-show-early-on-C-h    t)
           (which-key-idle-delay           most-positive-fixnum)
           (which-key-idle-secondary-delay 1e-100))
  :blackout t)

;; I want my Emacs to be like vim or spacemacs, where commands are
;; bound to certain prefixes by functional groupings. Unfortunately,
;; the `bind-key' built into `use-package' does not provide good
;; support. This is the reason for using general. Although this
;; increases the complexity.
(eval-when-compile
  ;; This wrap with `eval-when-compile' is not necessary, it only for
  ;; get rid of `flycheck' annoying message.
  (straight-use-package 'general))

(use-package general
  :straight t)

;; `hydra' is another visualization tool for key bindings, it used to
;; tie related commands into a family of short bindings with a common
;; prefix.
(use-package hydra
  :straight t)

;; `hydra' does not provide intergration with `use-package', unlike
;; `general'.  `use-package-hydra' provides this intergration, enable
;; it and register it as keyword of `use-package'.
(eval-when-compile
  ;; This wrap with `eval-when-compile' is not necessary, it only for
  ;; get rid of `flycheck' annoying message.
  (straight-use-package 'use-package-hydra))

(use-package use-package-hydra
  :straight t)

;; Define check commands map.
(require 'flycheck)
(defvar my-check-command-map (copy-keymap flycheck-command-map)
  "Keymap for my check commands that should be put under a prefix.")

(general-define-key
 :wk-full-keys nil
 :keymaps 'my-keymap
 "c" '(:keymap my-check-command-map :which-key "check"))

;;; Emacs:

;; Emacs have a powerful encoding system to deal with many coding
;; systems to particular regions, but I want use UTF-8 as default.
(setq locale-coding-system 'utf-8)
(when (fboundp 'set-charset-priority)
  (set-charset-priority 'unicode))
(prefer-coding-system 'utf-8)

;; For the reuse of the Emacs session, startup Emacs after the first
;; Emacs process start.
(use-package server
  :hook (after-init-hook . server-mode))

;; Emacs quit default method do not require confirmation, this is not
;; good.  Confirm with `y-or-n-p' when exit Emacs.
(setq confirm-kill-emacs 'y-or-n-p)

;; Emacs has a lot of key-bindings to use "C-x" and "C-c" as prefix,
;; but sometimes it can accidentally touch the "C-z" when press them.
;; It will suspend the Emacs, I do not need it.
(unbind-key "C-z" global-map)

;; `y-or-n-p' is one way that Emacs asks a yes-or-no question, usually
;; to get your confirmation for something you requested.

;; `yes-or-no-p' is another way that Emacs uses to do this.  It is
;; typically used for more important yes-or-no questions, that is,
;; questions where the answer might have more serious consequences.
;; You must type a full yes or no to answer `yes-or-no-p'.

;; By default, Emacs use the relatively conservative way (yes-or-no-p)
;; to confirm answer.  I want to always use `y-or-n-p'.
(fset 'yes-or-no-p 'y-or-n-p)

;; The Emacs default backup and autosave behavior is terrible, set
;; higher levels of backup and autosave, redirecting the cache files.
(use-package files
  :custom ((create-lockfiles nil)
           (make-backup-files nil)
           (auto-save-default t)

           ;; Change backup files location.
           (backup-by-copying t)
           (backup-directory-alist
            `((".*" . ,(concat my-data-dir "backup/"))))

           ;; Change autosave files location.
           (auto-save-file-name-transforms
            `((".*" ,(concat my-cache-dir "auto-save/") t)))
           (auto-save-list-file-prefix
            (concat my-cache-dir "auto-save/"))

           ;; Maximum and minimum number of versions.
           (version-control t)
           (delete-old-versions t)
           (kept-new-versions 6)
           (kept-old-versions 2)))

;; `super-save' can auto-save buffers when certain events happen.  Use
;; it to improve default save and backup.
(use-package super-save
  :straight t
  :hook (after-init-hook . super-save-mode)
  :custom ((super-save-auto-save-when-idle t)
           ;; Turn off super-save for remote files.
           (super-save-remote-files nil)
           ;; Exclude specific files from super-save.
           (super-save-exclude '(".gpg")))
  :config (add-to-list 'super-save-hook-triggers 'find-file-hook)
  :blackout t)

;; `recentf-mode' is a built-in minor mode that keeps track of the
;; files you have opened, allowing you to revisit them faster.  Its
;; true power consists in the fact that its data, maintained in
;; recentf-list, is a simple variable.  This means that we can access
;; it through any relevant piece of Elisp functionality.
(use-package recentf
  :blackout (recentf-mode)
  :hook (after-init-hook . recentf-mode)
  :custom ((recentf-max-saved-items 500)
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
                              "COMMIT_EDITMSG\\'"))))

;; When we need reuse Emacs, restore cursor position is useful.
;; `save-place-mode' can remember where the point is in any given
;; file.  This can often be a subtle reminder of what you were doing
;; the last time you visited that file, allowing you to pick up from
;; there.
(use-package saveplace
  :blackout (save-place-mode)
  :hook (after-init-hook . save-place-mode)
  :custom (save-place-forget-unreadable-files t))

;; I bind all buffer operations to a map, so that I can easily modify
;; the binding of this map.
(defvar my-buffer-command-map (make-sparse-keymap)
  "Keymap for my buffer commands that should be put under a prefix.")

(general-define-key
 :wk-full-keys nil
 :keymaps 'my-keymap
 "b" '(:keymap my-buffer-command-map :which-key "buffer"))

;; `buffer-move' provides swap buffers without typing C-x b on each
;; window.
(use-package buffer-move
  :straight t
  :general (:keymaps 'my-buffer-command-map
            :wk-full-keys nil
            "C-p" '(buf-move-up    :which-key "move to up")
            "C-n" '(buf-move-down  :which-key "move to down")
            "C-f" '(buf-move-right :which-key "move to right")
            "C-b" '(buf-move-left  :which-key "move to left")))

;; Define a map to bind the operations of project.
(defvar my-window-command-map (make-sparse-keymap)
  "Keymap for my window commands that should be put under a prefix.")

(general-define-key
 :wk-full-keys nil
 :keymaps 'my-keymap
 "w" '(:keymap my-window-command-map :which-key "window"))

;; `winner-mode' is a global minor mode, allow undo or redo changes in
;; the window configuration.
(use-package winner
  :blackout (winner)
  :general  (:keymaps 'my-window-command-map
             :wk-full-keys nil
             "u" '(winner-undo :which-key "undo")
             "r" '(winner-redo :which-key "redo"))
  :hook (after-init-hook . winner-mode)
  :custom (winner-dont-bind-my-keys t))

;; Emacs has a lot of window type, the one I use most often is the
;; popup.  I select to use `shackle' to manage the rules of buffers,
;; but it not support auto close by default.  A solution is use hack
;; from Centaur Emacs.
;; ref: https://github.com/seagle0128/.emacs.d
(use-package shackle
  :straight t
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
      (let (window buffer process)
        (if (one-window-p)
            (progn
              (setq window (selected-window))
              (when (equal (buffer-local-value
                            'shackle--current-popup-window
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
  :custom ((shackle-default-size 0.5)
           (shackle-default-alignment 'below)
           (shackle-default-rule nil)
           (shackle-rules my-buffer-rules))
  :init
  (put 'shackle--current-popup-window 'permanent-local t)
  (advice-add #'keyboard-quit :before #'shackle-close-popup-window-hack)
  (advice-add #'shackle-display-buffer :around #'shackle-display-buffer-hack)
  :blackout t)

;; By default, Emacs bind `other-window' to "C-x o".  It provides
;; switch window by cycle, but it does not work well.  Use
;; `ace-window' replace it.
(use-package ace-window
  :straight t
  :bind ([remap other-window] . ace-window)
  :config (when (featurep 'super-save)
            (add-to-list 'super-save-triggers 'ace-window))
  :blackout t)

;; `winum' is a similar package to `ace-window', it also provides a
;; set of window operations with different way.
(use-package winum
  :straight t
  :general (:keymaps 'my-window-command-map
            :wk-full-keys nil
            "0" '(winum-select-window-0 :which-key "goto window 0")
            "1" '(winum-select-window-1 :which-key "goto window 1")
            "2" '(winum-select-window-2 :which-key "goto window 2")
            "3" '(winum-select-window-3 :which-key "goto window 3")
            "4" '(winum-select-window-4 :which-key "goto window 4")
            "5" '(winum-select-window-5 :which-key "goto window 5")
            "6" '(winum-select-window-6 :which-key "goto window 6")
            "7" '(winum-select-window-7 :which-key "goto window 7")
            "8" '(winum-select-window-8 :which-key "goto window 8")
            "9" '(winum-select-window-9 :which-key "goto window 9")
            "`" '(winum-select-window-by-number
                  :which-key "select window"))
  :hook (after-init-hook . winum-mode)
  ;; :custom (winum-auto-setup-mode-line nil)
  )

;;; Interface:

(straight-use-package 'projectile)

(eval-when-compile
  (require 'projectile))

;; I need some project interface to manage my projects.  So define a
;; command map first.
(defvar my-project-command-map (copy-keymap projectile-command-map)
  "Keymap for my project commands that should be put under a prefix.")

(general-define-key
 :wk-full-keys nil
 :keymaps 'my-keymap
 "p" '(:keymap my-project-command-map :which-key "project"))

;; `projectile' is a project interaction library for Emacs.  Its goal
;; is to provide a nice set of features operating on a project level
;; without introducing external dependencies (when feasible).  For
;; instance - finding project files has a portable implementation
;; written in pure Emacs Lisp without the use of GNU find (but for
;; performance sake an indexing mechanism backed by external commands
;; exists as well).
(use-package projectile
  :straight t
  :hook (after-init-hook . projectile-mode)
  :general (:keymaps 'my-project-command-map
	    :wk-full-keys nil
	    "4" '(:ignore t :which-key "find other")
            "5" '(:ignore t :which-key "find other")
	    "s" '(:ignore t :which-key "grep")
            "x" '(:ignore t :which-key "shell"))
  :config
  (when (featurep 'ivy)
    (setq projectile-completion-system 'ivy))
  :blackout t)

;; `ibuffer' provides visualization buffer management, ensure binding
;; the IBuffer keymap, and customize face.
(use-package ibuffer
  :general ("C-x C-b" 'ibuffer
            [remap list-buffers] 'ibuffer)
  :custom
  (ibuffer-filter-group-name-face
   '(:inherit (font-lock-string-face bold)))
  :config
  (when (featurep 'counsel)
    (general-define-key
     :keymaps 'ibuffer-mode-map
     "C-x C-f" 'counsel-find-file)))

;; It is a good idea to sort the buffer displayed by `ibuffer' by
;; project. `ibuffer-projectile' provides show buffer list grouped by
;; `projectile'.
(use-package ibuffer-projectile
  :straight t
  :after (ibuffer projectile)
  :functions (ibuffer-projectile-set-filter-groups)
  :hook (ibuffer-mode-hook . ibuffer-projectile-set-filter-groups)
  :custom (ibuffer-projectile-prefix
           (if my-icons
               (concat (all-the-icons-octicon
                        "file-directory"
                        :face ibuffer-filter-group-name-face
                        :v-adjust 0.0
                        :height 1.0)
                       " ")
             "Project: "))
  :blackout t)

;; I do files and directories with `dired', so define a map to bind
;; these operations.
(defvar my-file-command-map (make-sparse-keymap)
  "Keymap for my file commands that should be put under a prefix.")

(general-define-key
 :wk-full-keys nil
 :keymaps 'my-keymap
 "f" '(:keymap my-file-command-map :which-key "file"))

;; `dired' can shows a directory (folder) listing that you can use to
;; perform various operations on files and subdirectories in this
;; directory.
(use-package dired
  :general (:keymaps 'my-file-command-map
            :wk-full-keys nil
            "d" '(dired :which-key "open directory"))
  :custom ((dired-recursive-deletes 'always)
           (dired-recursive-copies 'always)
           (ls-lisp-use-insert-directory-program t)
           ;; Show directory first.
           (dired-listing-switches "-alh --group-directories-first")))

;; `diredfl' provides colourful dired.
(use-package diredfl
  :straight t
  :hook (dired-mode-hook . diredfl-mode))

;; Use `dired-quick-sort' provides sort in `dired-mode'.
(use-package dired-quick-sort
  :straight t
  :general (:keymaps 'dired-mode-map
            "S" 'hydra-dired-quick-sort/body))

;; Use `dired-git-info' to show git info in `dired'.
(use-package dired-git-info
  :straight t
  :general (:keymaps 'dired-mode-map
            ")" 'dired-git-info-mode))

;;; Editor:

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

;; Emacs use `auto-revert-mode' provides buffers automatically
;; up-to-date.
(eval-when-compile
  (require 'autorevert))
(blackout auto-revert-mode)

;; Emacs supported multiple line wrap way as a powerful text editor,
;; includes these a few flavors:

;; 1. By default, Emacs wraps a line that reaches the window width,
;; except at a word boundary. The buffer text is not changed.

;; 2.Modes such as `auto-fill-mode' insert a line ending after the
;; last word that occurs before the value of option ‘fill-column’ (a
;; column number).

;; 3. Modes such as visual column (in concert with `visual-line-mode')
;; wrap a line after the last word before ‘fill-column’, but
;; ultimately they do not alter the buffer text. Such “soft” wrapping
;; is essentially a display effect.

;; 4.Modes such as `visual-line-mode' wrap a line right before the
;; window edge, but ultimately they do not alter the buffer
;; text. `visual-line-mode' wrapping is essentially a display effect.

;; 5. Mail read or written within Emacs can take advantage of
;; FormatFlowed

;; When I am writting, I want Emacs to wrap my excessively long lines
;; of text, but not actually perform line breaks.  But it should not
;; spill over into programming, so I only load `visual-line-mode' when
;; I really need it, and turn off `auto-fill-mode' by default.  That
;; means using the fourth way.

;; Inhibit display `visual-line-mode' in major/minor mode list.
(blackout visual-line-mode)

;; Normally when word-wrap is on, Emacs only breaks lines after
;; whitespace characters.  Whexn this option is turned on, Emacs also
;; breaks lines after characters that have the "|" category (defined
;; in `characters.el').  This is useful for allowing breaking after
;; CJK characters and improves the word-wrapping for CJK text mixed
;; with Latin text. It only available on Emacs 28 and above.
(unless (version< emacs-version "28.0")
  (setq word-wrap-by-category t))

;; By default, Emacs will goto the beginning of line when press
;; C-a. `mwim' provides better cursor jump experience.
(use-package mwim
  :straight t
  :general
  ([remap move-beginning-of-line] 'mwim-beginning-of-code-or-line
   [remap move-end-of-line]       'mwim-end-of-code-or-line))

;; Define a map to bind editor command.
(defvar my-edit-command-map (make-sparse-keymap)
  "Keymap for my editor commands that should be put under a prefix.")

(general-define-key
 :wk-full-keys nil
 :keymaps 'my-keymap
 "e" '(:keymap my-edit-command-map :which-key "edit"))

;; A Collection of Ridiculously Useful extensions for Emacs, I use
;; some find files and case conversion funtions that prvides.
(use-package crux
  :straight t
  :general
  (:keymaps 'my-file-command-map
   :wk-full-keys nil
   "C-d" '(crux-delete-file-and-buffer :which-key "delete current file")
   "r" '(crux-recentf-find-file :which-key "open recentf file")
   "m" '(:ignore t :which-key "my emacs")
   "m i" '(crux-find-user-init-file :which-key "open init file")
   "m c" '(crux-find-user-custom-file :which-key "open custom file"))
  (:keymaps 'my-buffer-command-map
   :wk-full-keys nil
   "K" '(crux-kill-other-buffers :which-key "kill other buffers"))
  (:keymaps 'my-edit-command-map
   :wk-full-keys nil
   "M-u" '(crux-upcase-region :which-key "upcase region")
   "M-l" '(curx-downcase-region :which-key "downcase region")
   "M-c" '(crux-capitalize-region :which-key "capitalize region")))

;; A frequently problem is how to edit files that without permissions
;; in Emacs.  Use `sudo-edit' to edit file under sudoer.
(use-package sudo-edit
  :straight t
  :general (:keymaps 'my-file-command-map
            :wk-full-keys nil
            "s" '(sudo-edit :which-key "sudo edit")
            "S" '(sudo-edit-find-file :which-key "sudo open")))

;; All modern editors should support simultaneous multi-line editing,
;; Emacs is includes in this list.  I use `multiple-cursors' to do
;; multiple edit.
;;
;; Example:
;;
;;  This is line 1.
;;  This is line 2.
;;  This is line 3.
;;  This is line 4.
;;
;; When the above-mentioned four rows area is selected, the effect of
;; `mc/edit-beginnings-of-lines', `mc/edit-ends-of-lines' and
;; `mc/mark-all-like-this' as follow.
;;
;;  This is line 1.
;; ^1           ^2 ^3
;;  This is line 2.
;; ^1           ^2 ^3
;;  This is line 3.
;; ^1           ^2 ^3
;;  This is line 4.
;; ^1           ^2 ^3
;;
;; Cursor will at "1" after execute `mc/edit-beginnings-of-lines'.
;; Cursor will at "2" after execute `mc/mark-all-like-this'.
;; Cursor will at "3" after execute `mc/edit-ends-of-lines'.
(use-package multiple-cursors
  :straight t
  :general (:keymaps 'my-edit-command-map
            :wk-full-keys nil
            "m"     '(:ignore t :which-key "multiple edit")
            "m c"   '(mc/edit-lines :which-key "edit lines")
            "m b"   '(mc/edit-beginnings-of-lines
                    :which-key "edit beginnings")
            "m e"   '(mc/edit-ends-of-lines :which-key "edit ends")
            "m l"   '(mc/mark-all-like-this :which-key "edit like")
            "m w"   '(mc/mark-all-words-like-this
                      :which-key "edit words")
            "m C-n" '(mc/insert-numbers :which-key "insert number")
            "m C-l" '(mc/insert-letters :which-key "insert letters")))

;; `undo-fu' is yet another `undo-tree'.
(use-package undo-fu
  :straight t
  :general (:keymaps 'my-edit-command-map
           "u" '(undo-fu-only-undo :which-key "undo")
           "R" '(undo-fu-only-redo :which-key "redo"))
  ([remap undo] 'undo-fu-only-undo))

;; `anzu' is an Emacs port of anzu.vim.  `anzu.el' provides a minor
;; mode which displays current match and total matches information in
;; the mode-line in various search modes.
(eval-when-compile
  ;; Get rid of `flycheck' warning.
  (straight-use-package 'anzu))

;; `browse-kill-ring' provides insert item from kill-ring.
(use-package browse-kill-ring
  :straight t
  :general (:keymaps 'my-edit-command-map
		     "k" '(browse-kill-ring :which-key "kill ring")))

;; `anzu' is an Emacs port of anzu.vim.  `anzu.el' provides a minor
;; mode which displays current match and total matches information in
;; the mode-line in various search modes.
(use-package anzu
  :straight t
  :hook (after-init-hook . global-anzu-mode)
  :general (:keymaps 'my-edit-command-map
            :wk-full-keys nil
            "r" '(:ignore t :which-key "replace")
            "r c" 'anzu-query-replace-at-cursor
            "r t" 'anzu-query-replace-at-cursor-thing)

  ([remap query-replace] 'anzu-query-replace
   [remap query-replace-regexp] 'anzu-query-replace-regexp)
  :blackout t)

;; `swiper' is an alternative to isearch that uses ivy to show an
;; overview of all matches.
(use-package swiper
  :straight counsel
  :general (:keymaps 'my-edit-command-map
            :wk-full-keys nil
            "s"   '(:ignore t  :which-key "search")
            "s s" '(swiper     :which-key "search")
            "s S" '(swiper-all :which-key "search in all buffers")))

;; I like use `occur', so add `replace+' for enhanced related support.
;; It provides shrink-wrapped to fit the matching lines that I need.
(use-package replace+
  :straight t)

;; `occur-x' adds some extra functionality to occur-mode. It allows
;; the user to refine any occur mode with extra regexp based filters.
(use-package occur-x
  :straight t
  :hook (occur-mode-hook . turn-on-occur-x-mode))

;; Define a map to bind navigation command.
(defvar my-goto-command-map (make-sparse-keymap)
  "Keymap for my navigation commands that should be put under a prefix.")

(general-define-key
 :wk-full-keys nil
 :keymaps 'my-keymap
 "g" '(:keymap my-goto-command-map :which-key "goto"))

;; `block-nav' provides some commands for navigating through code
;; based on indentation.
(use-package block-nav
  :straight '(block-nav :type git
                        :host github
                        :repo "nixin72/block-nav.el")
  :general (:keymaps 'my-goto-command-map
            :wk-full-keys nil
           "n"   '(block-nav-next-block
                   :which-key "next block")
           "p"   '(block-nav-previous-block
                   :Which-key "previous block")
           "C-n" '(block-nav-next-indentation-level
                   :which-key "next indentation")
           "C-p" '(block-nav-previous-indentation-level
                   :which-key "previous indentation")))

;; `avy' provides some commands that can jumping to visible text using
;; a char-based decision tree.
(use-package avy
  :straight t
  :general (:keymaps 'my-goto-command-map
            :wk-full-keys nil
            "c" 'avy-goto-char
            "l" 'avy-goto-line
            "w" 'avy-goto-word-1))

;; `imenu' produces menus for accessing locations in documents,
;; typically in the current buffer.
(use-package imenu
  :general (:keymaps 'my-goto-command-map
            :wk-full-keys nil
            "i" 'imenu))

;;; Programming:

;; Highlight current line when programming.
(use-package hl-line
  :hook (prog-mode-hook . hl-line-mode)
  :blackout (hl-line-mode))

;; Hightlight TODO and similar keywords in comments and strings when
;; programming.
(use-package hl-todo
  :straight t
  :general
  (:keymaps 'my-goto-command-map
   :wk-full-keys nil
   "N" '(hl-todo-next :which-key "next todo")
   "P" '(hl-todo-previous :which-key "previous todo"))
  :hook (prog-mode-hook . hl-todo-mode))

;; Wakatime service provides statistics and analysis of programming.
(use-package wakatime-mode
  :straight t
  :hook (after-init-hook . global-wakatime-mode)
  :blackout t)

;; Language Server Protocol (LSP) defines the protocol used between an
;; editor or IDE and a language server that provides language features
;; like auto complete, go to definition, find all references etc.

;; `lsp-mode' provides LSP support for Emacs, with LSP server,
;; frontend and user interface integration.
(use-package lsp-mode
  :when my-lsp
  :straight t
  ;; During Emacs startup, no packages explicitly depend on `lsp', so
  ;; explicitly defer loading it .
  :defer t
  :commands lsp
  :hook (lsp-mode-hook . lsp-enable-which-key-integration)
  :config
  ;; I need some `lsp-mode' interface to improve programming language
  ;; support.  So define a command map copy from `lsp-command-map'.
  (defvar my-lsp-command-map (copy-keymap lsp-command-map)
    "Keymap for my programming language LSP support.")

  (general-define-key
   :wk-full-keys nil
   :keymaps 'my-keymap
   "l" '(:keymap my-lsp-command-map :which-key "lsp"))

  :blackout t)

;; Hight level UI modules of `lsp'.
(use-package lsp-ui
  :when (featurep 'lsp-mode)
  :straight t
  :defer t
  :general
  ([remap xref-find-definitions] 'lsp-ui-peek-find-definitions
   [remap xref-find-references]  'lsp-ui-peek-find-references)
  :blackout t)

;;; Programming Language:

;;; Emacs Lisp:

;; `macroexpand', `macroexpand-1' and `macroexpand-all' will display
;; expansion of target macro in echo area by default.  Most of the
;; time macro expansion is so long that it can not be fully displayed
;; in that area.  And in fact I often need to determine if
;; `use-package' macro is expanding as expected, so I defined some
;; interactive functions to help to do it.
;;
;; ref: https://stackoverflow.com/questions/5925485
;;
;; When `major-mode' is `emacs-lisp-mode'.
;;
;; key             binding
;; ---             -------
;;
;; C-c e e         my-macroexpand
;; C-c e 1         my-macroexpand-1
;; C-c e a         my-macroexpand-all
(use-package macroexp
  :preface
  (defvar my-emacs-lisp-mode-command-map (make-sparse-keymap)
    "Keymap for my commands in `emacs-lisp-mode' under a prefix.")

  (defun my-macroexpand (sexp)
    "Display expansion of `macroexpand' at SEXP in new buffer."
    (interactive (list (sexp-at-point)))
    (with-output-to-temp-buffer "*emacs-lisp-macroexpand*"
      (pp (macroexpand sexp)))
    (with-current-buffer "*emacs-lisp-macroexpand*"
      (emacs-lisp-mode)))

  (defun my-macroexpand-1 (sexp)
    "Display expansion of `macroexpand-1' at SEXP in new buffer."
    (interactive (list (sexp-at-point)))
    (with-output-to-temp-buffer "*emacs-lisp-macroexpand*"
      (pp (macroexpand-1 sexp)))
    (with-current-buffer "*emacs-lisp-macroexpand*"
      (emacs-lisp-mode)))

  (defun my-macroexpand-all (sexp)
    "Display expansion of `macroexpand-all' at SEXP in new buffer."
    (interactive (list (sexp-at-point)))
    (with-output-to-temp-buffer "*emacs-lisp-macroexpand*"
      (pp (macroexpand-all sexp)))
    (with-current-buffer "*emacs-lisp-macroexpand*"
      (emacs-lisp-mode)))

  :general (:prefix "C-c"
            :keymaps 'emacs-lisp-mode-map
            "e"   '(:ignore t          :which-key "macro expand")
            "e e" '(my-macroexpand     :which-key "expand")
            "e 1" '(my-macroexpand-1   :which-key "expand 1")
            "e a" '(my-macroexpand-all :which-key "expand all")))

;;; TeX:

;; AUCTeX is THE TeX extension for Emacs.  Be careful with
;; configuration because it overrides the built-in tex package.
(use-package auctex
  :straight t
  :mode ("\\.tex\\'" . LaTeX-mode)
  :hook ((LaTeX-mode-hook . visual-line-mode)
	 (LaTeX-mode-hook . turn-on-reftex))
  :custom ((TeX-master nil)
	   (TeX-auto-save t)
	   (TeX-parse-self t)
	   (reftex-plug-into-AUCTeX t)
	   (TeX-command-list '(("LaTeX"
				"%`xelatex%(mode)%' %t"
				TeX-run-command nil t
				:help "Run XeLaTeX")
			       ("Tectonic" "tectonic %t"
				TeX-run-command nil t
				:help "Run tectonic")))
	   (TeX-view-program-selection '((output-pdf "pdf-tools")))
	   (TeX-view-program-list '(("pdf-tools" "TeX-pdf-tools-sync-view")))
	   (TeX-source-correlate-mode t)
	   (TeX-source-correlate-start-server t))
  :init (when my-lsp-tex
	  (add-hook 'LaTeX-mode-hook #'lsp))
  :blackout t)

;;; Tools:

;; `langtool' provides grammar, style, and spell check by use
;; languagetool.
(use-package langtool
  :straight t
  :commands (langtool-check
	     langtool-correct-buffer
	     langtool-show-message-at-point)
  :general (:keymaps 'my-check-command-map
	    :wk-full-keys nil
	    "C-l" '(langtool-check :which-key "language check")
	    "r"   '(langtool-correct-buffer :which-key "mark right")
	    "."   '(langtool-show-message-at-point
		    :which-key "langtool detail"))
  :custom
  ((langtool-java-classpath
    "/usr/share/languagetool:/usr/share/java/languagetool/*")
   (langtool-autoshow-message-function
    'langtool-suggestions)))

;; `pdf-tools' provides major mode for rendering PDF files, much
;; better than DocView, and has much richer set of features.
(use-package pdf-tools
  :straight (pdf-tools)
  :mode ("\\.pdf$" . pdf-view-mode)
  :hook (pdf-view-mode-hook . auto-revert-mode))

; Feature `straight-x' from package `straight' provides
;; experimental/unstable extensions to `straight' which are not yet
;; ready for official inclusion.  I use it update packages that is
;; used.
(use-package straight-x
  :commands (straight-x-fetch-all))

;;; Appearance:

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

;; By default, Emacs displays the current line number of the point in
;; the mode line.  In addition, display the current column is also
;; useful.
(setq line-number-mode     t
      column-number-mode   t
      size-indication-mode t)

;; Colorize color names in buffers.
;; #000000 #ffffff #00ff00 #ff0000 #0000ff
(use-package rainbow-mode
  :straight t
  :blackout (rainbow-mode)
  :hook (after-init-hook . rainbow-mode))

;; I have load `all-the-icons' in early initialization, but ensure it
;; again.
(use-package all-the-icons
  :when my-icons
  :straight t)

;; `company-posframe' let `company' use child frame as its candidate
;; menu, and also provides better documentation supprot when complete.
(use-package company-posframe
  :when my-posframe
  :straight t
  :hook (company-mode-hook . company-posframe-mode)
  :blackout t)

;; Documentation popups support for `company', this feature only will
;; be activated when `posframe' is unavailable.
(use-package company-quickhelp
  :unless my-posframe
  :straight t
  :hook (company-mode-hook . company-quickhelp-mode)
  :blackout t)

;; Enable `posframe' support of `hydra' if it is available.
;; `hydra-posframe' shows hydra hints on posframe.
(use-package hydra-posframe
  :when my-posframe
  :straight (hydra-posframe :type git
                            :host github
                            :repo "Ladicle/hydra-posframe")
  :hook (after-init-hook . hydra-posframe-mode)
  :custom (hydra-posframe-parameters '((left-fringe  . 12)
                                       (right-fringe . 12)))
  :blackout t)

;; `which-key' conveniently pop up a buffer at the bottom of the
;; frame, but for me moving my sight to the bottom interferes with
;; concentration.  Using `which-key-posfram', if the `posframe' is
;; available.
(use-package which-key-posframe
  :when my-posframe
  :straight t
  :hook (which-key-mode-hook . which-key-posframe-mode)
  :blackout t)

;; `dired-posframe' make `dired' use `posframe', it will preview file and show contents vai `posframe'.
(use-package dired-posframe
  :when my-posframe
  :straight t
  :hook (dired-mode-hook . dired-posframe-mode)
  :blackout t)

;; Display the files and directories with icons of `all-the-icons' in
;; `ibuffer-mode'.
(use-package all-the-icons-ibuffer
  :when my-icons
  :straight t
  :after ibuffer
  :hook (ibuffer-mode-hook . all-the-icons-ibuffer-mode)
  :blackout t)

;; Use `all-the-icons' in `dired-mode' by `all-the-icons-dired'.
(use-package all-the-icons-dired
  :when my-icons
  :straight (all-the-icons-dired
             :type   git
             :host   github
             :repo   "brsvh/all-the-icons-dired"
             :branch "patch")
  :hook (dired-mode-hook . all-the-icons-dired-mode)
  :custom-face
  (all-the-icons-dired-dir-face  ((t (:height 0.8))))
  (all-the-icons-dired-file-face ((t (:height 0.8))))
  :blackout t)

;;; Epilogue:

;; This section do some operations that must be done at the very end
;; of the `user-init-file'.

(provide 'init)
;;; init.el ends here
