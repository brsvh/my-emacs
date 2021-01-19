;;; early-init.el --- early initialization -*- lexical-binding: t; -*-

;; Copyright (C) 2020-2021 Burgess Chang

;; This file is not part of GNU Emacs.

;;; Commentary:

;; After Emacs 27, the early startup behaviors can be configured with
;; early-init.el.  This is the description for Emacs early intialize
;; file in Emacs manual.

;; Most customizations for Emacs should be put in the normal init
;; file.  See Init File.  However, it is sometimes desirable to have
;; customizations that take effect during Emacs startup earlier than
;; the normal init file is processed.  Such customizations can be put
;; in the early init file, $XDG_CONFIG_DIR/.config/emacs/early-init.el
;; or ~/.emacs.d/early-init.el.  This file is loaded before the
;; package system and GUI is initialized, so in it you can customize
;; variables that affect frame appearance as well as the package
;; initialization process, such as `package-enable-at-startup',
;; `package-load-list', and `package-user-dir'.

;; So in this file, I mainly do further configuration of UI and
;; package initialization.  About package initialization, I have
;; migrated to `straight' now.

;; To see the outline of this file, run M-x occur with the following
;; query:
;;   ^;;;;* \|^(

;;; Code:

;; Defer garbage collection further back in the startup process, this
;; will save time in garbage collection.
(setq gc-cons-threshold most-positive-fixnum)

;; In noninteractive sessions, prioritize non-byte-compiled source
;; files to prevent the use of stale byte-code.  Otherwise, it saves
;; us a little IO time to skip the mtime checks on every *.elc file.
(setq load-prefer-newer noninteractive)

;;; Advices:

(defun my--fix-display-graphic-p (func &optional display)
    "Apply DISPLAY to all formulated FUNC.
Fix `display-graphic-p' so it works while early initialization."
    (if display
        (funcall func display)
      ;; `display-graphic-p' lies by returning nil, but
      ;; `initial-window-system' tells the truth (it is nil only if I
      ;; am actually in a tty environment).
      initial-window-system))

;; I use `all-the-icons' to show more awesome icons, but I only
;; activate it during graphic interface.  So I need ensure load it by
;; `display-graphic-p', but this predicate can not work during early
;; initialization.
(advice-add #'display-graphic-p :around #'my--fix-display-graphic-p)

;;; Optimization:

(defvar my-initial-file-name-handler-alist file-name-handler-alist
  "A list used to restore `file-name-handler-alist'.")

(defun my-restore-file-name-handler-alist ()
  "Restore `file-name-handler-alist'.
Add existed `file-name-handler-alist' rather than directly setq
it, because it maybe changed."
  (dolist (handler file-name-handler-alist)
    (add-to-list 'my-initial-file-name-handler-alist handler))
  (setq file-name-handler-alist my-initial-file-name-handler-alist))

;; `file-name-handler-alist' is consulted on every require, load and
;; various path/io functions.  Get a minor speed up by nooping this.
(unless (or noninteractive (daemonp))
  ;; set `file-name-handler-alist' to empty.
  (setq file-name-handler-alist nil)
  ;; Restore `file-name-handler-alist' at the very end of startup.
  (add-hook 'emacs-startup-hook
            #'my-restore-file-name-handler-alist
            -100))

;; After Emacs 27, package initialization occurs before
;; `user-init-file' is loaded, but after `early-init-file'.  I don’t
;; use `package.el', so prevent its’ initialization.
(setq package-enable-at-startup nil)

;; Resizing the Emacs frame can be a terribly expensive part of
;; changing the font.  By inhibiting this, it easily halve startup
;; times with fonts that are larger than the system default.  This
;; optimization is borrowed from Doom Emacs early-init.el.
(setq frame-inhibit-implied-resize t)

;; Disable bidirectional text rendering for a modest performance
;; boost.  I've set this to `nil' in the past, but the
;; `bidi-display-reordering's docs say that is an undefined state and
;; suggest this to be just as good:
(setq bidi-display-reordering  'left-to-right
      bidi-paragraph-direction 'left-to-right)

;; Reduce rendering/line scan work for Emacs by not rendering cursors
;; or regions in non-focused windows.
(setq cursor-in-non-selected-windows nil
      highlight-nonselected-windows  nil)

;; More performant rapid scrolling over unfontified regions.  May
;; cause brief spells of inaccurate syntax highlighting right after
;; scrolling, which should quickly self-correct.
(setq fast-but-imprecise-scrolling t)

;; Font compacting can be terribly expensive, especially for rendering
;; icon fonts on Windows.  Whether disabling it has a notable affect
;; on Linux and Mac hasn't been determined, but I inhibit it there
;; anyway.  This increases memory usage, however!
(setq inhibit-compacting-font-caches t)

; Emacs "updates" its ui more often than it needs to, so I slow it down
;; slightly from 0.5s.
(setq idle-update-delay 1.0)

;; Introduced in Emacs HEAD (b2f8c9f), this inhibits fontification
;; while receiving input, which should help with performance while
;; scrolling.
(setq redisplay-skip-fontification-on-input t)

;; Remove command line options that aren't relevant to our current OS;
;; means slightly less to process at startup.
(setq command-line-x-option-alist nil)

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

(defconst my-straight-dir (concat my-cache-dir
                                  "straight/repos/straight.el/")
  "Directory for `straight' data storage.")

(message my-straight-dir)

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

;;; Package initialization:

;; Get rid of free variables reference.
(defvar straight-base-dir)
(defvar straight-repository-branch)
(defvar straight-build-dir)
(defvar straight-vc-git-default-clone-depth)
(defvar straight-use-symlinks)
(defvar straight-enable-package-integration)
(defvar straight-enable-use-package-integration)
(defvar straight-check-for-modifications)

;; Customize `straight' behaviors.
(setq straight-base-dir my-cache-dir
      ;; Use the develop branch of `straight'.
      straight-repository-branch "develop"
      ;; Reset the name of `straight' build directory include
      ;; `emacs-version' infomation to save build history.
      straight-build-dir (format "build-%s/" emacs-version)
      ;; Set clone depth to 1 when use git protocol.
      straight-vc-git-default-clone-depth 1
      ;; Use symlinks for building packages.
      straight-use-symlinks t
      ;; Enable integration with `package'.
      straight-enable-package-integration t
      ;; Enable integration with `use-package'.
      straight-enable-use-package-integration t)

;; If watchexec and Python are installed, use file watchers to detect
;; package modifications.  This saves time at startup.  Otherwise, use
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

;; Add `my-straight-dir' to `load-path'.
(push my-straight-dir load-path)

(require 'straight)

;; `straight' bootstrap.
(with-temp-buffer
  (insert-file-contents (concat my-straight-dir "bootstrap.el"))
  (eval-region (search-forward "(require 'straight)") (point-max)))

;; Whether Emacs built-in packages or third-party packages are
;; prefered put their configuration and cache files to
;; `user-emacs-directory'.  Because I follow the XDG Base
;; Specification, I need change their behaviors.  `no-littering' has a
;; collection of such configuration, directly use it.

;; After setting the values of `no-littering-etc-directory' and
;; `no-littering-var-directory', strange etc/ and var/ file
;; directories are created under `user-emacs-directory', even though
;; the values of the variables are as expected.  The var/ directory
;; includes company/ and emacs-session/ directories, so my guess is
;; that I loaded `company' at the beginning caused it. Resolve it by
;; move `no-littering' loading to here.
(defvar no-littering-etc-directory)
(defvar no-littering-var-directory)
(setq no-littering-etc-directory my-data-dir
      no-littering-var-directory my-cache-dir)
(straight-use-package '(no-littering
			:type git
			:host github
			:repo "emacscollective/no-littering"))
(require 'no-littering)

;;; Appearance:

;; My Emacs is built with the gtk3 toolkit.  By default, Emacs will
;; set the font to value from the global environment.  But my font is
;; different on a different machine, and I preferred to use Consolas
;; on all cases when edit.

;; Some functions like `set-face-attribute', `set-face-background' and
;; `set-face-foreground' have performance issue, it expensive at which
;; frame needs update.  So avoid using these functions.
(push '(font . "Consolas") default-frame-alist)

;; Emacs will render an 80x55 frame by default, reducing the height to
;; 40.
(push '(height . 50) default-frame-alist)
(push '(width  . 120) default-frame-alist)

;; Each Emacs buffer normally has a menu bar at the top, it can use to
;; perform common operations.  I don't need it.
(push '(menu-bar-lines . nil) default-frame-alist)

;; `tool-bar-mode' provides some buttons like most other tool bars, I
;; don't need it too.
(push '(tool-bar-lines . nil) default-frame-alist)

;; Inhibit the vertical/horizontal scrool bar in Emacs.
(push '(vertical-scroll-bars   . nil) default-frame-alist)
(push '(horizontal-scroll-bars . nil) default-frame-alist)

;; The default frame title is what I don't like, reset it.
(setq frame-title-format
      '(buffer-file-name "%f" (dired-directory dired-directory "%b")))

;; Setting this values will force one-line scrolling everywhere (mouse
;; and keyboard), resulting most of the times in a smoother scrolling
;; than the actual smooth scrolling.
(setq scroll-step           1
      scroll-conservatively 10000)

(setq mouse-wheel-scroll-amount     '(1 ((shift) . 1))
      mouse-wheel-progressive-speed nil
      mouse-wheel-follow-mouse      't)

;; The Modus themes are designed for accessible readability.  They
;; conform with the highest standard for color contrast between any
;; given combination of background and foreground values.  This
;; corresponds to the WCAG AAA standard, which specifies a minimum
;; rate of distance in relative luminance of 7:1.
(straight-use-package 'modus-themes)
(require 'modus-themes)

 ;; Customizations of modus theme.
(setq modus-themes-slanted-constructs t
      modus-themes-bold-constructs    t
      modus-themes-mode-line          '3d)

;; Prevent the glimpse when change the default themes.
(modus-themes-load-themes)
(modus-themes-load-operandi)

;; `all-the-icons.el' is a utility package to collect various Icon
;; Fonts and propertize them within Emacs.
(when (display-graphic-p)
  (straight-use-package 'all-the-icons)
  (require 'all-the-icons))

;; Avoid messing with things more than necessary.
(advice-remove #'display-graphic-p #'my--fix-display-graphic-p)

(provide 'early-init)
;;; early-init.el ends here
