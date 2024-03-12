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

(use-my writing-markdown)

(use-my writing-org)

(provide 'my-writing)
;;; my-writing.el ends here
