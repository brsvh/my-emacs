;;; transient.el --- transient configuration -*- lexical-binding: t; -*-

;; Redirect `transient' files.
(setq transient-history-file
      (expand-file-name "transient/history.el" user-data-directory)
      transient-levels-file
      (expand-file-name "transient/levels.el" user-data-directory)
      transient-values-file
      (expand-file-name "transient/values.el" user-data-directory))

;;; transient.el ends here
