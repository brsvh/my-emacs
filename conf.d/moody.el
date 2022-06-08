;;; moody.el --- moody configuration -*- lexical-binding: t; -*-

;; Activate Moody Mode Line.
(add-hook 'emacs-startup-hook
          #'(lambda ()
              (moody-replace-mode-line-buffer-identification)
              (moody-replace-vc-mode)
              (moody-replace-eldoc-minibuffer-message-function)
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

;;; moody.el ends here