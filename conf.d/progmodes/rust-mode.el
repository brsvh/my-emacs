;;; rust-mode.el --- Rust Programming configuration -*- lexical-binding: t; -*-

;; Enable LSP(eglot) support.
(add-hook 'rust-mode-hook 'eglot-ensure)

(with-eval-after-load 'rust-mode
  (define-keys
   :keymaps 'rust-mode-map
   :prefix "C-c m"
   "!" '(rust-check               :which-key "check")
   "c" '(rust-compile             :which-key "compile")
   "d" '(rust-dbg-wrap-or-unwrap  :which-key "debug")
   "f" '(rust-format-buffer       :which-key "format")
   "l" '(rust-run-clippy          :which-key "clippy")
   "n" '(rust-goto-format-problem :which-key "next format problem")
   "r" '(rust-run                 :which-key "run")
   "t" '(rust-test                :which-key "test"))

  ;; Inhibit default bindings in `rust-mode'.
  (define-keys
   :keymaps 'rust-mode-map
   "C-c C-c" nil
   "C-c C-d" nil
   "C-c C-f" nil
   "C-c C-n" nil))

;;; rust-mode.el ends here
