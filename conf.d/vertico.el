;;; vertico.el --- vertico configuration -*- lexical-binding: t; -*-

;; Grow and shrink the Vertico minibuffer
(setq vertico-resize t)

;; enable cycling for `vertico-next' and `vertico-previous'.
(setq vertico-cycle t)

;; More convenient directory navigation commands.
(define-keys
 :keymaps 'vertico-map
 "RET"   '(vertico-directory-enter       :which-key "enter")
 "DEL"   '(vertico-directory-delete-char :which-key "delete char")
 "M-DEL" '(vertico-directory-delete-word :which-key "delete word"))

;; Tidy shadowed file names.
(add-hook 'rfn-eshadow-update-overlay-hook 'vertico-directory-tidy)

;; Activate Mouse support of `vertico'.
(add-hook 'vertico-mode-hook 'vertico-mouse-mode)

(vertico-mode +1)

;;; vertico.el ends here
