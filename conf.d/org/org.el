;;; org.el --- org configuration -*- lexical-binding: t; -*-

;; Inhibit keeps tags aligned when modifying headlines.
(setq org-auto-align-tags nil)

;; Place tags directly after headline text.
(setq org-tags-column 0)

;; Catch invisible region.
(setq org-catch-invisible-edits 'show-and-error)

;; Back the cursor to the beginning of the headline text when press
;; C-a, and jump to the end of the headline when press C-e, ignoring
;; the presence of tags in the headline.
(setq org-special-ctrl-a/e t)

;; Perfer insert new headings after the current subtree.
(setq org-insert-heading-respect-content t)

;; Hide emphasis marker.
(setq org-hide-emphasis-markers t)

;; show entities as UTF8 characters.
(setq org-pretty-entities t)

;; Change ellipsis to U+2026.
(setq org-ellipsis "…")

;; Indent text according to outline structure.
(setq org-startup-indented t)

;;; org.el ends here