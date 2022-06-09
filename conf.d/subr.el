;;; subr.el --- Enhance basic lisp subroutines for Emacs -*- lexical-binding: t; -*-

(define-keys
 :keymaps 'global-map
 :prefix "C-x"
 "RET" '(:ignore t :which-key "m17n")
 "4"   '(:ignore t :which-key "window")
 "5"   '(:ignore t :which-key "frame")
 "6"   '(:ignore t :which-key "two column")
 "8"   '(:ignore t :which-key "character")
 "a"   '(:ignore t :which-key "abbrev")
 "f"   '(:ignore t :which-key "file")
 "n"   '(:ignore t :which-key "narrow")
 "p"   '(:ignore t :which-key "project")
 "r"   '(:ignore t :which-key "register")
 "t"   '(:ignore t :which-key "tab")
 "v"   '(:ignore t :which-key "vc")
 "x"   '(:ignore t :which-key "buffer")
 "C-k" '(:ignore t :which-key "kmacro"))

(define-keys
 :keymaps 'global-map
 :prefix "C-c"
 "m"   '(:ignore t :which-key "major mode")
 "v"   '(:ignore t :which-key "version control"))

;;; subr.el ends here
