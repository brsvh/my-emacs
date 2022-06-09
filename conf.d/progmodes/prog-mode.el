;;; prog-mode.el --- programming mode configuration -*- lexical-binding: t; -*-

;; Use Corfu as completion function.
(add-hook 'prog-mode-hook 'corfu-mode)

;; Highlight matched paren.
(add-hook 'prog-mode-hook 'show-paren-mode)

;; Automatic parenthesis pairing.
(add-hook 'prog-mode-hook 'electric-pair-mode)

;; Highlight current line.
(add-hook 'prog-mode-hook 'hl-line-mode)

;; Display line numbers.
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

;;; prog-mode.el ends here
