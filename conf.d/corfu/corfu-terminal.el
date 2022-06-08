;;; corfu-terminal.el --- corfu in terminal configuration -*- lexical-binding: t; -*-

;; Use corfu in non-graphical interface.
(unless (display-graphic-p)
  (corfu-terminal-mode +1))

;;; corfu-terminal.el ends here