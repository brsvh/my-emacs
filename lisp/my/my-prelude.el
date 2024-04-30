;;; my-prelude.el --- Prelude of My Emacs  -*- lexical-binding: t -*-

;; Copyright (C) 2022-2024 Burgess Chang

;; Author: Burgess Chang <bsc@brsvh.org>
;; Keywords: local
;; Package-Requires: ((emacs "29.1") (my-lib "0.1.0"))
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

;; The prelude of my Emacs configuration, setup matters of utmost
;; importance before initialization.

;; This file should be loaded at the `early-init-file' or the very
;; beginning of either the `user-init-file'.

;;; Code:

(defconst my-etc-directory
  (expand-file-name "etc/" user-emacs-directory)
  "Directory beneath which my configuration files are placed.")

(defconst my-lisp-directory
  (expand-file-name "lisp/" user-emacs-directory)
  "Directory beneath which my Emacs Lisp files are placed.")

(defconst my-native-lisp-directory
  (expand-file-name "native-lisp/" user-emacs-directory)
  "Directory beneath which third-party Emacs Lisp files are placed.")

(defconst my-site-lisp-directory
  (expand-file-name "site-lisp/" user-emacs-directory)
  "Directory beneath which third-party Emacs Lisp files are placed.")

(defconst my-prelude-eln-load-path
  (let ((base (if (not (memq system-type '(ms-dos windows-nt cygwin)))
                  (or (getenv "XDG_CACHE_HOME") "~/.cache/")
                user-emacs-directory)))
    (list
     (expand-file-name "emacs/eln-cache/" base)
     my-native-lisp-directory))
  "Directory to look for natively-compiled *.eln files.")

(defvar my-prelude-file-name-handler-alist file-name-handler-alist
  "The initial value of `file-name-handler-alist'.")

(defvar my-prelude-frame-alist '((menu-bar-lines . nil)
                                 (tool-bar-lines . nil)
                                 (vertical-scroll-bars . nil)
                                 (horizontal-scroll-bars . nil))
  "Frame parameters for setup frame creation.")

(defvar my-prelude-inhibit-update-load-path nil
  "Inhibit update `load-path` if non-nil.")

(defvar my-prelude-load-path
  (list my-lisp-directory my-site-lisp-directory)
  "List of directories to search for self-maintained Emacs Lisp files.")

(defun my-prelude-add-subdirs-to-load-path (dir)
  "Add subdirectories of DIR to `load-path'."
  (let ((default-directory dir))
    (normal-top-level-add-subdirs-to-load-path)))

(defun my-prelude-reset-gc-threshold ()
  "Reset `consult--gc-threshold'."
  (let ((default (default-value 'gc-cons-threshold)))
    (setq gc-cons-threshold default)))

(defun my-prelude-restore-file-name-handler ()
  "Restore the appropriate value of `file-name-handler-alist'."
  (let ((default my-prelude-file-name-handler-alist)
        (prev file-name-handler-alist))
    (setq file-name-handler-alist
          (delete-dups (append default prev)))))

(defun my-prelude-update-load-path (&rest _)
  "Update necessary paths to `load-path'."
  (dolist (dir (nreverse my-prelude-load-path))
    (push dir load-path)
    (my-prelude-add-subdirs-to-load-path dir)))



(progn
  ;; Add customized paths for user Emacs Lisp Native Compilation file
  ;; storage.
  (dolist (path (reverse my-prelude-eln-load-path))
    (setq native-comp-eln-load-path
          (cons path native-comp-eln-load-path)))

  ;; Let my Emacs Lisp files can be find.
  (unless my-prelude-inhibit-update-load-path
    (my-prelude-update-load-path))

  (require 'cl-lib)
  (require 'my-lib)

  (cl-eval-when (compile)
    (require 'benchmark-init)
    (require 'gcmh))

  ;; Avoid garbage collection during the initialization to achieve a
  ;; faster startup.
  (setq gc-cons-threshold most-positive-fixnum)

  ;; Restore the garbage collection threshold to its default value.
  (add-hook 'after-init-hook #'my-prelude-reset-gc-threshold 100)

  ;; Activate intelligent garbage collections.
  (unless (fboundp 'gcmh-mode)
    (autoload #'gcmh-mode "gcmh" nil t))
  (add-hook 'after-init-hook #'gcmh-mode 100)

  ;; Skip consulting the `file-name-handler-alist' to save overhead.
  (setq file-name-handler-alist nil)

  ;; Restore the appropriate value of `file-name-handler-alist`.
  (add-hook 'after-init-hook #'my-prelude-restore-file-name-handler 100)

  ;; Activate `benchmark-init' as early as possible to capture loading
  ;; information during the startup process.
  (unless (fboundp 'benchmark-init/activate)
    (autoload #'benchmark-init/activate "benchmark-init" nil t))
  (benchmark-init/activate)

  ;; Deactivate `benchmark-init' at the very end of `after-init-hook'.
  (unless (fboundp 'benchmark-init/deactivate)
    (autoload #'benchmark-init/deactivate "benchmark-init" nil t))
  (add-hook 'after-init-hook #'benchmark-init/deactivate 100)

  ;; Inhibit save customizations to `user-init-file`.
  (setq custom-file my-custom-file)

  ;; Redirect the location of directory containing the user’s Emacs Lisp
  ;; packages.
  (setq package-user-dir (my-data-path (format "elpa/%s/" emacs-version))
        package-quickstart-file (my-path package-user-dir "autoloads.el"))

  ;; Reset frame layout.
  (setq default-frame-alist
        (delete-dups (append default-frame-alist
                             my-prelude-frame-alist))))

(provide 'my-prelude)
;;; my-prelude.el ends here
