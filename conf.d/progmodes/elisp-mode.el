;;; elisp-mode.el --- Emacs Lisp Programming configuration -*- lexical-binding: t; -*-

;; Inhibit add new line at the end of Emacs Lisp files.
(add-hook 'emacs-lisp-mode-hook
          #'(lambda () (setq-local require-final-newline nil)))

;;; elisp-mode.el ends here