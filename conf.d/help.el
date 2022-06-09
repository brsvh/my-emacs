;;; help.el --- help configuration -*- lexical-binding: t; -*-

(define-keys
 :keymaps 'global-map
 :prefix "C-h"
 "."     '(display-local-help               :which-key "local help")
 "4"     '(:ignore t                        :which-key "window")
 "?"     '(help-for-help                    :which-key "help")
 "a"     '(apropos-command                  :which-key "apropos")
 "b"     '(describe-bindings                :which-key "bindings")
 "c"     '(describe-key-briefly             :which-key "key (briefly)")
 "C"     '(describe-coding-system           :which-key "coding system")
 "d"     '(apropos-documentation            :which-key "documentation")
 "e"     '(view-echo-area-messages          :which-key "messages")
 "f"     '(describe-function                :which-key "function")
 "F"     '(Info-goto-emacs-command-node     :which-key "node info (Emacs)")
 "g"     '(describe-gnu-project             :which-key "gnu")
 "h"     '(view-hello-file                  :which-key "hello")
 "i"     '(info                             :which-key "info (Top)")
 "I"     '(describe-input-method            :which-key "input method")
 "k"     '(describe-key                     :which-key "key")
 "K"     '(Info-goto-emacs-key-command-node :which-key "key info (Emacs)")
 "l"     '(view-lossage                     :which-key "lossage")
 "L"     '(describe-language-environment    :which-key "language environment")
 "m"     '(describe-mode                    :which-key "mode")
 "n"     '(view-emacs-news                  :which-key "news")
 "o"     '(describe-symbol                  :which-key "symbol")
 "p"     '(finder-by-keyword                :which-key "keyword")
 "P"     '(describe-package                 :which-key "package")
 "q"     '(help-quit                        :which-key "quit")
 "r"     '(info-emacs-manual                :which-key "info (Emacs)")
 "R"     '(info-display-manual              :which-key "info")
 "s"     '(describe-syntax                  :which-key "syntax")
 "t"     '(help-with-tutorial               :which-key "tutorial")
 "v"     '(describe-variable                :which-key "variable")
 "w"     '(where-is                         :which-key "where is")
 "x"     '(describe-command                 :which-key "command")
 "<f1>"  '(help-for-help                    :which-key "help")
 "C-h"   nil
 "C-a"   '(about-emacs                      :which-key "about Emacs")
 "C-c"   '(describe-copying                 :which-key "copying")
 "C-d"   '(view-emacs-debugging             :which-key "debug")
 "C-e"   '(view-external-packages           :which-key "info (Emacs) elpa")
 "C-f"   '(view-emacs-FAQ                   :which-key "FAQ")
 "RET"   '(view-order-manuals               :which-key "info (Emacs) order")
 "C-n"   '(view-emacs-news                  :which-key "news")
 "C-o"   '(describe-distribution            :which-key "distribution")
 "C-p"   '(view-emacs-problems              :which-key "problems")
 "C-r"   '(restart-emacs                    :which-key "restart Emacs")
 "C-s"   '(search-forward-help-for-help     :which-key "search help")
 "C-t"   '(view-emacs-todo                  :which-key "todo")
 "C-w"   '(describe-no-warranty             :which-key "warranty")
 "C-\\"  '(describe-input-method            :which-key "input method"))

(define-keys
 :keymaps 'global-map
 :prefix "C-h 4"
 "i"     '(info-other-window                :which-key "info"))

;;; help.el ends here
