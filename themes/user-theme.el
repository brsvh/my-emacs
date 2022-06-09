;;; user-theme.el --- user theme control -*- lexical-binding: t; -*-

;; Use `spacemacs-light' as default theme.
(setq user-default-theme 'spacemacs-light
      user-light-theme 'spacemacs-light
      user-dark-theme 'spacemacs-dark)

(add-hook 'user-theme-load-hook
          #'(lambda ()
              ;; Set more subtle line numbers.
              (let ((bg (face-attribute 'default :background))
                    (fg (face-attribute 'default :foreground)))
                (set-face-attribute
                 'line-number nil :background bg)
                (set-face-attribute
                 'line-number-current-line nil :foreground fg)
                (set-face-attribute
                 'line-number-current-line nil :weight 'bold))
              ;; Set borderless mode line.
              (let ((line (face-attribute 'mode-line :underline)))
                (set-face-attribute
                 'mode-line          nil :overline   line)
                (set-face-attribute
                 'mode-line-inactive nil :overline   line)
                (set-face-attribute
                 'mode-line-inactive nil :underline  line)
                (set-face-attribute
                 'mode-line          nil :box        nil)
                (set-face-attribute
                 'mode-line-inactive nil :box        nil))))

;;; user-theme.el ends here
