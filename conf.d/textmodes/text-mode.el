;;; text-mode.el --- Text Mode configuration -*- lexical-binding: t; -*-

;; Highlight current line when edit text file.
(add-hook 'text-mode-hook 'hl-line-mode)

;; Auto wrap line when edit text file.
(add-hook 'text-mode-hook 'visual-line-mode)

;;; text-mode.el ends here
