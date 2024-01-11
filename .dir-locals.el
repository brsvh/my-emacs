;;; Directory Local Variables            -*- no-byte-compile: t -*-
;;; For more information see (info "(emacs) Directory Variables")

((auto-mode-alist . (("melpa/[^/]*$" . lisp-data-mode)))
 (lisp-data-mode . ((indent-tabs-mode . nil)
                    (sentence-end-double-space . t)))
 (prog-mode . ((eval . (display-fill-column-indicator-mode 1)))))
