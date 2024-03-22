;;; my-dired.el --- Customize `dired' for My Emacs  -*- lexical-binding: t -*-

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

(require 'my-core)

(use-package dired
  :config
  (setq dired-dwim-target t)

  (setq dired-recursive-deletes 'always
        dired-recursive-copies 'always)

  (setq dired-listing-switches
        (concat "-l "
                "--almost-all "
                "--human-readable "
                "--group-directories-first "
                "--no-group")))

(use-package dired-aux
  :after dired
  :demand t)

(use-package dired-x
  :after dired
  :config
  (let ((call (cond ((my-os-is linux) "xdg-open")
                    (t ""))))
    (setq dired-guess-shell-alist-user
          `(("\\.\\(?:djvu\\|eps\\)\\'" ,call)
            ("\\.\\(?:jpg\\|jpeg\\|png\\|gif\\|xpm\\)\\'" ,call)
            ("\\.\\(?:mp3\\|flac\\)\\'" ,call)
            ("\\.\\(?:mp4\\|mkv\\|avi\\|flv\\|rm\\|rmvb\\|ogv\\)\\(?:\\.part\\)?\\'" ,call)
            ("\\.\\(?:xcf\\)\\'" ,call)
            ("\\.csv\\'" ,call)
            ("\\.docx\\'" ,call)
            ("\\.html?\\'" ,call)
            ("\\.md\\'" ,call)
            ("\\.tex\\'" ,call)
            ("\\.pdf\\'" ,call))))
  :demand t)

(use-package diredfl
  :vc (:url "https://github.com/purcell/diredfl.git")
  :hook
  (dired-mode-hook . diredfl-mode))

(use-package dired-git-info
  :ensure dired-git-info
  :after dired
  :keymap-set
  (:dired-mode-map
   (")" . dired-git-info-mode)))

(use-package nerd-icons-dired
  :when (display-graphic-p)
  :vc (:url "https://github.com/rainstormstudio/nerd-icons-dired.git")
  :hook
  (dired-mode-hook . nerd-icons-dired-mode))

(provide 'my-dired)
;;; my-dired.el ends here
