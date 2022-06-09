;;; cus-edit.el --- customize tools configuration -*- lexical-binding: t; -*-

;; Redirect save location of easy customize storage.
(setq custom-file (expand-file-name "custom.el" user-data-directory))

;; Load `custom-file' after `user-init-file' is loaded.
(add-hook 'after-init-hook #'(lambda () (load custom-file nil t)))

;;; cus-edit.el ends here
