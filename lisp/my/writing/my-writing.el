;;; my-writing.el --- Writing support of My Emacs -*- lexical-binding: t -*-

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

(use-package outline
  :hook
  (outline-mode-hook . hl-line-mode))

(use-package text-mode
  :hook
  (text-mode-hook . hl-line-mode))

(use-package outline
  :hook
  (outline-mode-hook . visual-line-mode))

(use-package text-mode
  :hook
  (text-mode-hook . visual-line-mode))

(use-package elec-pair
  :hook
  (outline-mode-hook . electric-pair-local-mode)
  (text-mode-hook . electric-pair-local-mode))

(use-package valign
  :vc (:url "https://github.com/casouri/valign.git")
  :config
  (setq valign-fancy-bar t))

(use-package pdf-view
  :ensure pdf-tools
  :pin nongnu
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :magic ("%PDF" . pdf-view-mode)
  :config
  (define-advice pdf-view-mode
    (:around (func &rest args) ensure-epdfinfo)
    "Ensure epdfinfo is build after the first PDF file."
    (if (and (require 'pdf-info nil t)
          (or (pdf-info-running-p)
           (ignore-errors (pdf-info-check-epdfinfo) t)))
      (apply func args)
      (fundamental-mode)
      (message "Viewing PDFs in Emacs requires epdfinfo. Use `M-x pdf-tools-install' to build it")))

  (pdf-tools-install-noverify)

  (-snocq shackle-rules          
          '("^\\*Outline*" :select nil :align right :size 0.4 :regexp t)))

(use-package pdf-annot
  :ensure pdf-tools
  :pin nongnu
  :config
  (-snocq shackle-rules
          '("\\(?:^\\*Contents\\|'s annots\\*$\\)" :select t :align below :size 0.4 :other t :popup t :regexp t)
          '("^\\*Edit Annotation "                 :select t :inhibit-window-quit t :regexp t)))

(use-my writing-markdown)

(use-my writing-org)

(provide 'my-writing)
;;; my-writing.el ends here
