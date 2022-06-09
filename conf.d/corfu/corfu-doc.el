;;; corfu-doc.el --- corfu documentation configuration -*- lexical-binding: t; -*-

(add-hook 'corfu-mode-hook #'corfu-doc-mode)

(define-keys
 :keymaps 'corfu-map
 "M-p" '(corfu-doc-scroll-down :which-key "scroll down")
 "M-n" '(corfu-doc-scroll-up   :which-key "scroll up")
 "M-d" '(corfu-doc-toggle      :which-key "toggle doc"))

;;; corfu-doc.el ends here
