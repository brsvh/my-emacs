;;; my-prelude.el --- Prelude of My Emacs  -*- lexical-binding: t -*-

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

(defmacro use-my (name &rest args)
  "NAME and ARGS are as in `use-package'."
  (declare (indent defun))
  (let ((feat (intern (concat "my-" (symbol-name name)))))
    `(use-package ,feat ,@args :demand t)))

(defvar my--load-path
  (list (expand-file-name "lisp/" user-emacs-directory)
        (expand-file-name "site-lisp/" user-emacs-directory))
  "List of directories to search for my files.")

(defun my--add-subdirs-to-load-path (dir)
  "Add subdirectories of DIR to `load-path'."
  (let ((default-directory dir))
    (normal-top-level-add-subdirs-to-load-path)))

(defun my--update-load-path (&rest _)
  "Update necessary paths to `load-path'."
  (dolist (dir (nreverse my--load-path))
    (push dir load-path)
    (my--add-subdirs-to-load-path dir)))

(defun my--load-custom-file (&rest _)
  "Load `custom-file'."
  (when (file-exists-p custom-file)
    (load custom-file nil 'nomessage)))

(use-my lib
  :preface
  (my--update-load-path))

(use-package startup
  :no-require t
  :init
  (startup-redirect-eln-cache my-eln-cache-directory))

(use-package use-package
  :init
  (require 'use-package-keymap)

  (setq use-package-always-defer t)

  (setq use-package-hook-name-suffix nil))

(use-package emacs
  :no-require t
  :init
  (let ((my-frame-alist '((menu-bar-lines . nil)
                          (tool-bar-lines . nil)
                          (vertical-scroll-bars . nil)
                          (horizontal-scroll-bars . nil))))
    (setq default-frame-alist
          (delete-dups (append default-frame-alist my-frame-alist)))))

(use-package cus-edit
  :init
  (setq custom-file my-custom-file)
  (my--load-custom-file))

(use-package package
  :init
  (setq package-user-dir my-elpa-directory
        package-quickstart-file (my-path my-elpa-directory "autoloads.el"))
  :config
  (add-to-list 'package-archives
               '("gnu-devel" . "https://elpa.gnu.org/devel/")
               'append)

  (add-to-list 'package-archives
               '("nongnu-devel" . "https://elpa.nongnu.org/nongnu-devel/")
               'append)

  (add-to-list 'package-archives
               '("melpa" . "https://melpa.org/packages/")
               'append))

(provide 'my-prelude)
;;; my-prelude.el ends here
