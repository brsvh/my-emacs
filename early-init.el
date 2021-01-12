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
;; package initialization.

;;; Code:

;; Defer garbage collection further back in the startup process, this
;; will save time in garbage collection.
(setq gc-cons-threshold most-positive-fixnum)

;; In noninteractive sessions, prioritize non-byte-compiled source
;; files to prevent the use of stale byte-code. Otherwise, it saves us
;; a little IO time to skip the mtime checks on every *.elc file.
(setq load-prefer-newer noninteractive)

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
;; various path/io functions. Get a minor speed up by nooping this.
(unless (or noninteractive (daemonp))
  ;; set `file-name-handler-alist' to empty.
  (setq file-name-handler-alist nil)
  ;; Restore `file-name-handler-alist' at the very end of startup.
  (add-hook 'emacs-startup-hook
            #'my-restore-file-name-handler-alist
            -100))

;; After Emacs 27, package initialization occurs before `user-init-file'
;; is loaded, but after `early-init-file'. I don’t use `package.el', so
;; prevent its’ initialization.
(setq package-enable-at-startup nil)

;; My Emacs is built with the gtk3 toolkit. By default, Emacs will set
;; the font to value from the global environment. But my font is
;; different on a different machine, and I preferred to use Consolas
;; on all cases when edit.

;; Some functions like `set-face-attribute', `set-face-background' and
;; `set-face-foreground' have performance issue, it expensive at which
;; frame needs update.  So avoid using these functions.
(push '(font . "Consolas") default-frame-alist)

;; Emacs will render an 80x55 frame by default, reducing the height to
;; 40.
(push '(height . 40) default-frame-alist)
(push '(width  . 80) default-frame-alist)

;; Each Emacs buffer normally has a menu bar at the top, it can use to
;; perform common operations. I don't need it.
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

;; Resizing the Emacs frame can be a terribly expensive part of
;; changing the font. By inhibiting this, we easily halve startup
;; times with fonts that are larger than the system default. This
;; optimization is borrowed from Doom Emacs early-init.el.
(setq frame-inhibit-implied-resize t)

;; Disable bidirectional text rendering for a modest performance
;; boost. I've set this to `nil' in the past, but the
;; `bidi-display-reordering's docs say that is an undefined state and
;; suggest this to be just as good:
(setq bidi-display-reordering  'left-to-right
      bidi-paragraph-direction 'left-to-right)

;; Reduce rendering/line scan work for Emacs by not rendering cursors
;; or regions in non-focused windows.
(setq cursor-in-non-selected-windows nil
      highlight-nonselected-windows  nil)

;; Font compacting can be terribly expensive, especially for rendering
;; icon fonts on Windows. Whether disabling it has a notable affect on
;; Linux and Mac hasn't been determined, but we inhibit it there
;; anyway. This increases memory usage, however!
(setq inhibit-compacting-font-caches t)

;; Don't blink the paren matching the one at point, it's too
;; distracting.
(setq blink-matching-paren nil)

;; Setting this values will force one-line scrolling everywhere (mouse
;; and keyboard), resulting most of the times in a smoother scrolling
;; than the actual smooth scrolling.
(setq scroll-step           1
      scroll-conservatively 10000)

(setq mouse-wheel-scroll-amount     '(1 ((shift) . 1))
      mouse-wheel-progressive-speed nil
      mouse-wheel-follow-mouse      't)

;; Prevent the glimpse when change the default themes.
(load-theme 'modus-operandi t)

;; Prevent the glimpse of modeline when Emacs startup.
(setq mode-line-format nil)

(provide 'early-init)
;;; early-init.el ends here
