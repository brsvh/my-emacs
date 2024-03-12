;;; my-writing-org.el --- Writing with `org-mode' -*- lexical-binding: t -*-

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

(use-package ox-md
  :after org
  :config
  (define-advice org-md-template
      (:around (orig-fun &rest args) export-title-as-hlevel-1)
    "An advice for export TITLE as the level 1 heading line."
    (let* ((res (apply orig-fun args))
           (title (org-element-interpret-data (plist-get (nth 1 args)
                                                         :title))))
      (if org-md-export-title-as-hlevel-1
          (concat "# " title "\n\n" res)
        res)))

  (setq org-md-toplevel-hlevel 2))

(use-package valign
  :after org
  :hook
  (org-mode-hook . valign-mode))

(provide 'my-writing-org)
;;; my-writing-org.el ends here
