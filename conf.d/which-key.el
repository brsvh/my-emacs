;;; which-key.el --- which-key configuration  -*- lexical-binding: t; -*-

(setq which-key-sort-order #'which-key-key-order-alpha
      which-key-sort-uppercase-first nil
      which-key-add-column-padding 1
      which-key-max-display-columns nil
      which-key-sort-uppercase-first nil
      ;; Allow C-h to trigger `which-key' before it is done
      ;; automatically.
      which-key-show-early-on-C-h t
      which-key-side-window-max-height 0.3)

(which-key-mode +1)

;;; which-key.el ends here