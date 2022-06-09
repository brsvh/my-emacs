;;; magit.el --- magit configuration -*- lexical-binding: t; -*-

;; Unbind useless key to `global-map'.
(setq global-magit-file-mode nil)

(define-keys
 :keymaps 'global-map
 :prefix "C-c v"
 "g"    '(:ignore t      :which-key "git")
 "g a"  '(magit-dispatch :which-key "git actions")
 "g s"  '(magit-status   :which-key "git status"))

;;; magit.el ends here
