;;; my-workspace.el --- Workspace support for My Emacs -*- lexical-binding: t -*-

;; Copyright (C) 2022-2024 Burgess Chang

;; Author: Burgess Chang <bsc@brsvh.org>
;; Keywords: local
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

(use-my lib)

(use-package tabspaces
  :vc (:url "https://github.com/mclear-tools/tabspaces.git")
  :config
  (setq tabspaces-use-filtered-buffers-as-default t)

  (setq tabspaces-default-tab "Default")

  (setq tabspaces-remove-to-default t)

  (setq tabspaces-include-buffers '("*scratch*" "*Messages*"))

  (setq tabspaces-session t
        tabspaces-session-file (my-state-path "tabspaces/sessons.eld"))

  (setq tabspaces-session-auto-restore t)

  (with-eval-after-load 'consult
    (consult-customize consult--source-buffer :hidden t :default nil)

    (defvar consult--source-workspace
      (list :name     "Workspace Buffer"
            :narrow   ?w
            :history  'buffer-name-history
            :category 'buffer
            :state    #'consult--buffer-state
            :default  t
            :items    (lambda () (consult--buffer-query
                                  :predicate #'tabspaces--local-buffer-p
                                  :sort 'visibility
                                  :as #'buffer-name)))
      "Set workspace buffer list for consult-buffer.")

    (add-to-list 'consult-buffer-sources 'consult--source-workspace))
  :hook
  (on-init-ui-hook . tabspaces-mode))

(provide 'my-workspace)
;;; my-workspace.el ends here
