;;; saveplace.el --- saveplace configuration -*- lexical-binding: t; -*-

;; Turn on Save Place mode.
(save-place-mode +1)

;; Redirect place file.
(setq save-place-file
      (expand-file-name "file-place.el" user-data-directory))

;;; saveplace.el ends here
