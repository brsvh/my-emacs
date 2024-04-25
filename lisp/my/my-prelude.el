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

(defconst my-site-lisp-directory
  (expand-file-name "site-lisp/" user-emacs-directory)
  "Directory beneath which third-party Emacs Lisp files are placed.")

(defconst my-prelude--eln-path
  (let ((base (if (not (memq system-type '(ms-dos windows-nt cygwin)))
                 (or (getenv "XDG_CACHE_HOME") "~/.cache/")
                user-emacs-directory)))
    (expand-file-name "emacs/eln-cache/" base))
  "Directory to look for natively-compiled *.eln files.")

(defvar my-prelude--frame-alist '((menu-bar-lines . nil)
                                  (tool-bar-lines . nil)
                                  (vertical-scroll-bars . nil)
                                  (horizontal-scroll-bars . nil))
  "Frame parameters for setup frame creation.")

(defvar my-prelude--inhibit-update-load-path nil
  "Inhibit update `load-path` if non-nil.")

(defvar my-prelude--load-path
  (list my-lisp-directory my-site-lisp-directory)
  "List of directories to search for self-maintained Emacs Lisp files.")

(defun my-prelude--add-subdirs-to-load-path (dir)
  "Add subdirectories of DIR to `load-path'."
  (let ((default-directory dir))
    (normal-top-level-add-subdirs-to-load-path)))

(defun my-prelude--update-load-path (&rest _)
  "Update necessary paths to `load-path'."
  (dolist (dir (nreverse my-prelude--load-path))
    (push dir load-path)
    (my-prelude--add-subdirs-to-load-path dir)))



(progn
  ;; Remove the default user Emacs Lisp Native Compilation file storage
  ;; path, e.g. (expand-file-name "eln-cache/" user-emacs-directory).
  (setq native-comp-eln-load-path (cdr native-comp-eln-load-path))

  ;; Set a customized path for user Emacs Lisp Native Compilation file
  ;; storage.
  (push my-prelude--eln-path native-comp-eln-load-path)

  ;; Let my Emacs Lisp files can be find.
  (unless my-prelude--inhibit-update-load-path
    (my-prelude--update-load-path))

  (require 'my-lib)

  ;; Inhibit save customizations to `user-init-file`.
  (setq custom-file my-custom-file)

  ;; TODO I use Nix to manage all Emacs Lisp packages which obtain from
  ;;      ELPAs, and my `user-emacs-directory' is located within the Nix
  ;;      Store.  Nevertheless, I still expect to be able to install
  ;;      additional Emacs Lisp packages through `package'.  Return here
  ;;      to fix the `package-user-dir' when appropriate.  However, for
  ;;      now, prevent the initialization of `package' at early init.
  (setq package-enable-at-startup nil)

  (with-eval-after-load 'package
    (eval-when-compile (defvar package-archives))

    ;; Add GNU Devel ELPA.
    (add-to-list 'package-archives
                 '("gnu-devel" . "https://elpa.gnu.org/devel/")
                 'append)

    ;; Add NonGNU Devel ELPA.
    (add-to-list 'package-archives
                 '("nongnu-devel" . "https://elpa.nongnu.org/nongnu-devel/")
      'append))

  ;; Reset frame layout.
  (setq default-frame-alist
        (delete-dups (append default-frame-alist
                             my-prelude--frame-alist))))

(provide 'my-prelude)
;;; my-prelude.el ends here
