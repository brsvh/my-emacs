;;; flymake.el --- on-the-fly syntax checker configuration -*- lexical-binding: t; -*-

(with-eval-after-load 'flymake
  (define-keys
   :keymaps 'flymake-mode-map
   :prefix "C-c"
   "!" '(consult-flymake :which-key "flymake diagnostic")))

(add-hook 'flymake-mode-hook
          #'(lambda ()
              (appendq minions-direct '(flymake-mode))))

;;; flymake.el ends here