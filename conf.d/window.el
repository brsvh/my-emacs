;;; window.el --- window configuration -*- lexical-binding: t; -*-

;; Configure base display rule of Emacs.
(appendq display-buffer-alist
         '(("\\*Backtrace\\*"
            (display-buffer-reuse-window
             display-buffer-in-side-window)
            (side . bottom)
            (window-height . 0.3)
            (slot . 1)
            (dedicated . t))
           ("\\*Compile-Log\\*"
            (display-buffer-reuse-window
             display-buffer-in-side-window)
            (side . bottom)
            (window-height . 0.3)
            (slot . 1)
            (dedicated . t))
           ("\\*Faces\\*"
            (display-buffer-reuse-window
             display-buffer-in-side-window)
            (side . bottom)
            (window-height . 0.3)
            (slot . 1)
            (dedicated . t))
           ("\\*Messages\\*"
            (display-buffer-reuse-window
             display-buffer-in-side-window)
            (side . bottom)
            (window-height . 0.3)
            (slot . 1)
            (dedicated . t))
           ("\\*Warnings\\*"
            (display-buffer-reuse-window
             display-buffer-in-side-window)
            (side . bottom)
            (window-height . 0.3)
            (slot . 1)
            (dedicated . t))))

;;; window.el ends here
