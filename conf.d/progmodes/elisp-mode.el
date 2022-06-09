;;; elisp-mode.el --- Emacs Lisp Programming configuration -*- lexical-binding: t; -*-

;; Inhibit show line numbers when `lisp-interaction-mode'.
(add-hook 'lisp-interaction-mode-hook
          #'(lambda ()
              (display-line-numbers-mode -1)))

;;; elisp-mode.el ends here
