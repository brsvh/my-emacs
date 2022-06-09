;;; startup.el --- startup control -*- lexical-binding: t; -*-

;; Inhibit create *GNU Emacs* buffer.
(setq inhibit-startup-screen t)

;; Inhibit content in *scratch* buffer.
(setq initial-scratch-message nil)

;; Redirect storage for Emacs session.
(setq auto-save-list-file-prefix
      (concat (expand-file-name "sessions/" user-data-directory)
              "session-"))

;;; startup.el ends here
