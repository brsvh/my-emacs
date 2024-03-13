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

(use-my lib)

(use-package dirvish
  :vc (:url "https://github.com/alexluigit/dirvish.git")
  :init
  (eval-after-load 'dired
    (dirvish-override-dired-mode))
  :config
  (setq dirvish-cache-dir (my-cache-path* "dirvish/")
        dirvish-hide-details nil
        dirvish-attributes '(git-msg)))

(use-package dirvish-vc
  :vc (:url "https://github.com/alexluigit/dirvish.git"
       :lisp-dir "extensions/"))

(provide 'my-dired)
;;; my-dired.el ends here
