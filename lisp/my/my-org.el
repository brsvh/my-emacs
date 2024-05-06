;;; my-org.el --- Org support of My Emacs -*- lexical-binding: t -*-

;; Copyright (C) 2022-2024 Burgess Chang

;; Author: Burgess Chang <bsc@brsvh.org>
;; Keywords: extensions
;; Package-Requires: ((emacs "29.1"))
;; URL: https://github.com/brsvh/my-emacs
;; Version: 0.1.50

;; This file is part of my-emacs.

;; my-emacs is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the
;; Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.

;; my-emacs is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with my-emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This file configures how I use `org-mode'.

;;; Code:

(require 'my-core)

(cl-eval-when (compile)
  (require 'elec-pair)
  (require 'org)
  (require 'org-capture)
  (require 'org-clock)
  (require 'org-id)
  (require 'org-indent)
  (require 'org-modern)
  (require 'org-persist)
  (require 'org-registry)
  (require 'org-side-tree)
  (require 'ox-publish)
  (require 'valign)
  (require 'window))



;;;
;; Core:

(setup org
  (:autoload org-mode)
  (:when-loaded
    (:set
     org-directory (my-path "~/org"))))

(setup org-indent
  (:autoload org-indent-mode))



;;;
;; Appearance:

(setup org-side-tree
  (:autoload org-side-tree)
  ;; Show a indented side tree.
  (:with-hook org-side-tree-mode-hook
    (:hook org-indent-mode)))

(setup valign
  (:autoload valign-mode))

(setup org
  (:when-loaded
    (:set
     ;; Inhibit alignment of tags.
     org-auto-align-tags nil

     ;; Hide markers.
     org-hide-emphasis-markers t

     ;; Insert new headings after the current subtree.
     org-insert-heading-respect-content t

     ;; Show pretty entities.
     org-pretty-entities t

     ;; Move What I Mean.
     org-special-ctrl-a/e t

     ;; Insert tags after heading.
     org-tags-column 0)
    (:with-map org-mode-map
      (:keymap-set
       ;; Show a side tree as outline.
       "C-c m t" #'org-side-tree))))

(setup org-modern
  (:autoload org-modern-mode)
  (:with-hook org-mode-hook
    (:hook org-modern-mode))
  (:when-loaded
    (:set
     org-modern-star '( "●" "◉" "◎" "⊙" "○")
     ;; Prefer to use `valign'.
     org-modern-table nil))
  ;; Align variable-pitch font, CJK characters and images in tables.
  (:with-hook org-modern-mode-hook
    (:hook valign-mode)))



;;;
;; Capture:

(setup window
  (:set
   (append display-buffer-alist)
   '("\\*Org Select\\*"
     ;; Prefer to show the select window under the current window.
     (display-buffer-reuse-window display-buffer-below-selected)
     ;; Hide Mode Line.
     (window-parameters (mode-line-format . none)))))



;;;
;; Clock:

(setup org-clock
  (:when-loaded
    (:set
     org-clock-persist-file (my-data-path "org/presist" "clock.el"))))



;;;
;; Editing:

(setup org
  (:with-hook org-mode-hook
    (:hook
     ;; Auto insert paried '*', '/', '=', and '~'.
     (lambda ()
       (electric-pair-local-mode +1)
       (:snoc electric-pair-pairs
              (cons ?* ?*)
              (cons ?/ ?/)
              (cons ?= ?=)
              (cons ?~ ?~))))))



;;;
;; id:

(setup org-id
  (:when-loaded
    (:set
     org-id-locations-file (my-data-path "org/" "id-locations.el"))))



;;;
;; Persist:

(setup org-persist
  (:when-loaded
    (:set
     org-persist-directory (my-data-path "org/persist/"))))



;;;
;; Publish:

(setup ox-publish
  (:when-loaded
    (:set
     org-publish-timestamp-directory (my-data-path "org/timestamps/"))))



;;;
;; Registry:

(setup org-registry
  (:when-loaded
    (:set
     org-registry-file (my-data-path "org/"  "registry.el"))))



(provide 'my-org)
;;; my-org.el ends here
