;;; recentf.el --- recentf configuration -*- lexical-binding: t; -*-

;; Redirect recent files records.
(setq recentf-save-file
      (expand-file-name "recent-files.el" user-data-directory))

(with-eval-after-load 'recentf
  (add-to-list 'recentf-exclude user-data-directory)
  (add-to-list 'recentf-exclude user-cache-directory))

(recentf-mode +1)

(define-advice recentf-load-list
    (:around (fn &rest args) silence-message)
  "Silencing load message."
  (cl-letf (((symbol-function #'message) #'ignore))
    (apply fn args)))

(define-advice recentf-cleanup
    (:around (fn &rest args) silence-message)
  "Silencing clean up message."
  (cl-letf (((symbol-function #'message) #'ignore))
    (apply fn args)))

(define-keys
 :keymaps 'global-map
 :prefix "C-x f"
 ""  nil
 "0"   '(recentf-open-most-recent-file-0 :which-key "most recent file (0)")
 "1"   '(recentf-open-most-recent-file-1 :which-key "most recent file (1)")
 "2"   '(recentf-open-most-recent-file-2 :which-key "most recent file (2)")
 "3"   '(recentf-open-most-recent-file-3 :which-key "most recent file (3)")
 "4"   '(recentf-open-most-recent-file-4 :which-key "most recent file (4)")
 "5"   '(recentf-open-most-recent-file-5 :which-key "most recent file (5)")
 "6"   '(recentf-open-most-recent-file-6 :which-key "most recent file (6)")
 "7"   '(recentf-open-most-recent-file-7 :which-key "most recent file (7)")
 "8"   '(recentf-open-most-recent-file-8 :which-key "most recent file (8)")
 "9"   '(recentf-open-most-recent-file-9 :which-key "most recent file (9)")
 "r"   '(consult-recent-file             :which-key "recent files")
 "C-r" '(recentf-open-files              :which-key "recent files buffer"))

;;; recentf.el ends here
