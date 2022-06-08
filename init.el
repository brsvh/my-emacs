;;; init.el --- Init File -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Burgess Chang

;; Author: Burgess Chang <bsc@brsvh.org>
;; License: GNU General Public License v3.0 or later
;; Package-Requires: ((emacs "28.1"))
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Version: 0.1

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This file is first loaded file after Emacs is started, we set up
;; the essential dependencies and preset configuration for our Emacs
;; integrated computing environment.

;;; Code:

;; Inhibit create *GNU Emacs* buffer.
(setq inhibit-startup-screen t)

;; Inhibit content in *scratch* buffer.
(setq initial-scratch-message nil)

;; Inhibit Menu Bar.
(menu-bar-mode -1)

;; Inhibit Tool Bar.
(tool-bar-mode -1)

;; Inhibit Scroll Bar.
(scroll-bar-mode -1)

;; Start server at startup.
(add-hook 'after-init-hook
          #'(lambda ()
              (eval-when-compile (require 'server))
              (unless (server-running-p) (server-start))))

;; Just use spaces.
(setq-default indent-tabs-mode nil)

;; Inhibit add new line at the end of Emacs Lisp files.
(add-hook 'emacs-lisp-mode-hook
          #'(lambda () (setq-local require-final-newline nil)))

;; Inhibit add new line at the end of Lisp Data files.
(add-hook 'lisp-data-mode-hook
          #'(lambda () (setq-local require-final-newline nil)))

(provide 'init)
;;; init.el ends here