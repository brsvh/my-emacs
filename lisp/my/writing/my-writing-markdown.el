;;; my-writing-markdown.el --- Writing with `markdown-mode' -*- lexical-binding: t -*-

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

(use-package markdown-mode
  :ensure markdown-mode
  :mode
  ("\\.markdown\\'" . markdown-mode)
  ("\\.md\\'" . markdown-mode)
  ("\\.mdown\\'" . markdown-mode)
  ("\\.mkd\\'" . markdown-mode)
  ("\\.mkdn\\'" . markdown-mode)
  ("README\\.md\\'" . gfm-mode)
  :config
  (setq markdown-command "multimarkdown"))

(use-package edit-indirect
  :ensure edit-indirect)

(use-package valign
  :after markdown-mode
  :hook
  (gfm-mode-hook . valign-mode)
  (markdown-mode-hook . valign-mode))

(provide 'my-writing-markdown)
;;; my-writing-markdown.el ends here
