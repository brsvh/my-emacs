;;; corfu.el --- corfu configuration -*- lexical-binding: t; -*-

;; Enable cycling for `corfu-next' and `corfu-previous'.
(setq corfu-cycle t)

;; Enable auto completion
(setq corfu-auto t
      corfu-quit-no-match 'separator)

(with-eval-after-load 'corfu
  ;; Activate icon support.
  (require 'kind-icon)
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

;; Remember selected candidates and to improve sorting.
(add-hook 'corfu-mode-hook 'corfu-history-mode)

;;Completing with Corfu in the minibuffer.
(add-hook 'minibuffer-setup-hook
          #'(lambda ()
              (when (where-is-internal #'completion-at-point
			               (list (current-local-map)))
                (corfu-mode +1))))

;;; corfu.el ends here
