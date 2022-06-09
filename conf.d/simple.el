;;; simple.el --- simple configuration -*- lexical-binding: t; -*-

;; Enable optional features of mode line.
(line-number-mode +1)
(column-number-mode +1)
(size-indication-mode +1)

;; Do not saves duplicates in kill-ring
(setq kill-do-not-save-duplicates t)

;; Enable better splitting words support of CJK characters.
(setq word-wrap-by-category t)

;;; simple.el ends here
