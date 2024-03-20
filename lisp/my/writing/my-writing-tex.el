;;; my-writing-tex.el --- Writing with TeX -*- lexical-binding: t -*-

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

(use-package tex
  :ensure auctex
  :pin gnu
  :init
  (setq TeX-auto-local ".auctex-auto"
        TeX-auto-save t
        TeX-electric-sub-and-superscript t
        TeX-source-correlate-method 'synctex
        TeX-source-correlate-mode t
        TeX-source-correlate-start-server nil
        TeX-style-local ".auctex-style"
        TeX-save-query nil
        TeX-parse-self t)
  :config
  (-snocq popper-reference-buffers
          "^\\*TeX \\(?:Help\\|errors\\)"
          " output\\*$")

  (setq TeX-master t)

  (-snocq TeX-command-list
          '("XeLaTeX" "xelatex -interaction=nonstopmode %s" TeX-run-command t t :help "Run XeLaTeX")))

(use-package latex
  :ensure auctex
  :pin gnu
  :mode
  ("\\.tex\\'" . LaTeX-mode))

(use-package cdlatex
  :ensure cdlatex
  :pin nongnu
  :config
  (setq cdlatex-use-dollar-to-ensure-math nil))

(provide 'my-writing-tex)
;;; my-writing-tex.el ends here
