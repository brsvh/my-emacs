;;; Directory Local Variables            -*- no-byte-compile: t -*-
;;; For more information see (info "(emacs) Directory Variables")

((auto-mode-alist . (("README\\'" . gfm-mode)
                     ("README.md\\'" . gfm-mode))) 
 (lisp-data-mode . ((indent-tabs-mode . nil)
                    (sentence-end-double-space . t)))
 (nil . ((eval . (display-fill-column-indicator-mode 1))
         (fill-column . 72))))
