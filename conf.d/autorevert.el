;;; autorevert.el --- autorevert configuration -*- lexical-binding: t; -*-

;; Revert buffers when the underlying file has changed.
(global-auto-revert-mode +1)

;; Revert Dired and other buffers
(setq global-auto-revert-non-file-buffers t)

;;; autorevert.el ends here
