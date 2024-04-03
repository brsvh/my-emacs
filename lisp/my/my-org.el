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

;; TODO describe this file.

;;; Code:

(require 'my-core)

;; Appearance:

(use-package org
  :ensure org
  :config
  (setq org-directory (my-path* "~/org"))

  (setq org-auto-align-tags nil
        org-hide-emphasis-markers nil
        org-pretty-entities t
        org-special-ctrl-a/e t
        org-startup-with-inline-images t
        org-tags-column 0)

  (setq org-export-allow-bind-keywords t)

  (-snocq display-buffer-alist
          '("\\*Org Select\\*"
            (display-buffer-reuse-window
             display-buffer-below-selected)
            (window-parameters (mode-line-format . none)))))

(use-package org-clock
  :ensure org
  :config
  (setq org-clock-persist-file (my-data-path* "org/"
                                              "clock-presist.el")))

(use-package org-id
  :ensure org
  :config
  (setq org-id-locations-file (my-data-path* "org/" "id-locations.el")))

(use-package org-persist
  :ensure org
  :config
  (setq org-persist-directory (my-data-path* "org/" "persist/")))

(use-package org-registry
  :ensure org-contrib
  :config
  (setq org-registry-file (my-data-path* "org/"  "registry.el")))

(use-package ox-publish
  :ensure org
  :config
  (setq org-publish-timestamp-directory (my-data-path* "org/"
                                                       "timestamps/")))

(use-package org-modern
  :ensure org-modern
  :after org
  :config
  (setq org-modern-table nil)

  (setq org-modern-star '( "●" "◉" "◎" "○" "◌"))
  :hook
  (org-mode-hook . org-modern-mode)
  (org-agenda-finalize-hook . org-modern-mode))

(use-package org-side-tree
  :ensure org-side-tree
  :after org
  :commands
  (org-side-tree)
  :keymap-set
  (:org-mode-map
   ("C-c m o" . org-side-tree))
  :hook
  (org-side-tree-mode-hook . org-indent-mode))



;; Editing:

(use-package elec-pair
  :after org
  :config
  (defun my--electric-pairs-for-org-mode (&rest _)
    "Update `electric-pair-pairs' when `org-mode'."
    (when (bound-and-true-p electric-pair-mode)
      (-snocq-local electric-pair-pairs
                    (cons ?= ?=)
                    (cons ?~ ?~)
                    (cons ?/ ?/)
                    (cons ?* ?*))))

  (add-hook 'org-mode-hook #'my--electric-pairs-for-org-mode))



;; Org Roam:

(use-package org-roam
  :ensure org-roam
  :config
  (setq org-roam-directory (my-path* org-directory "roam/")))

(provide 'my-org)
;;; my-org.el ends here
