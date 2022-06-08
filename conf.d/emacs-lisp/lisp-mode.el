;;; lisp-mode.el --- Lisp configuration -*- lexical-binding: t; -*-

;; Inhibit add new line at the end of Lisp Data files.
(add-hook 'lisp-data-mode-hook
          #'(lambda () (setq-local require-final-newline nil)))

;;; lisp-mode.el ends here