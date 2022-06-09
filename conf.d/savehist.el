;;; savehist.el --- savehist configuration -*- lexical-binding: t; -*-

;; Turn on Save History mode
(savehist-mode +1)

;; Redirect minibuffer history file.
(setq savehist-file
      (expand-file-name "minibuffer-history.el" user-data-directory))

;;; savehist.el ends here
