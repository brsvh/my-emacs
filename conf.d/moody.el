;;; moody.el --- moody configuration -*- lexical-binding: t; -*-

;; Activate Moody Mode Line.
(add-hook 'emacs-startup-hook
          #'(lambda ()
              (moody-replace-mode-line-buffer-identification)
              (moody-replace-vc-mode)
              (moody-replace-eldoc-minibuffer-message-function)))

;;; moody.el ends here
