;;; files.el --- files configuration -*- lexical-binding: t; -*-

;; Configure the behavior of files and directories, including basic
;; file visiting, backup generation, ITS-id version control.

(with-eval-after-load 'files
  (let ((default-directory user-data-directory))
    (make-directory (expand-file-name "auto-save/") 'parents)
    (make-directory (expand-file-name "backup/") 'parents)))

(setq auto-save-default t
      ;; Don't auto-disable auto-save after deleting big chunks.
      ;; This defeats the purpose of a failsafe.
      auto-save-include-big-deletions t)

;; Change default transforms to apply to buffer file name before
;; making auto-save file name.
(prependq auto-save-file-name-transforms
          `((".*"
             ,(expand-file-name "auto-save/" user-data-directory)
             t)))

;; Create backup files for each modified file on saving, and enable
;; make multiple numbered backup files.
(setq make-backup-files t
      version-control t)

;; Make backup files using the old file replication.
(setq backup-by-copying t)

;; Adjust the threshold for automatic deletion of versioned backup
;; files.
(setq delete-old-versions t
      kept-old-versions 5
      kept-new-versions 5)

;; Prevent create backup files in-place, alternative create them in
;; `user-data-directory'.
(prependq backup-directory-alist
          `(("."
             . ,(expand-file-name "backup/" user-data-directory))))

;;; files.el ends here