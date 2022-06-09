;;; server.el --- server configuration -*- lexical-binding: t; -*-

;; Start server at startup.
(add-hook 'after-init-hook
          #'(lambda ()
              (eval-when-compile (require 'server))
              (unless (server-running-p) (server-start))))

;;; server.el ends here
