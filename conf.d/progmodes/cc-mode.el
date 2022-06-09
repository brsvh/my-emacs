;;; cc-mode.el --- C and similar languages programming configuration -*- lexical-binding: t; -*-

;; Enable LSP(eglot) support.
(add-hook 'c++-mode-hook 'eglot-ensure)

;;; cc-mode.el ends here
