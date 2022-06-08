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

(require 'cl-macs)
(require 'user)

(defun init-package-initialize (&rest _)
  "Load all files which describe packages to be used."
  (eval-when-compile (require 'package))
  (let* ((default-directory user-config-directory)
         (quelpa (expand-file-name "quelpa/" user-cache-directory))
         (lisp (expand-file-name "lisp/"))
         (site-lisp (expand-file-name "site-lisp/"))
         (dot-regex directory-files-no-dot-files-regexp)
         ;; (rx string-start ".pkgs.el" string-end)
         (pkgs-regex "\\`\\.pkgs\\.el\\'")
         (desc-files
          (directory-files-recursively default-directory pkgs-regex))
         (local-pkgs '()))
    (setq quelpa-dir (format "%s%s/" quelpa emacs-version)
	  quelpa-update-melpa-p nil)
    (cl-loop for desc-file in desc-files
             do (load desc-file nil 'nomessage))
    (cl-letf (((symbol-function #'message) #'ignore))
      (package-install-selected-packages 'noconfirm)
      (require 'quelpa)
      (quelpa-setup-p))
    (setq local-pkgs
	  (nconc (directory-files lisp 'full dot-regex)
                 (directory-files site-lisp 'full dot-regex)))
    (cl-loop for local-pkg in local-pkgs
             when (or (file-directory-p local-pkg)
                      (string-match "\\`[^z-a]*?\\.el\\'" local-pkg))
             do (let ((pkg (intern (file-name-base local-pkg))))
                  (unless (package-installed-p pkg)
                    (quelpa
                     `(,pkg :fetcher file :path ,local-pkg)))))))

(defun init-config-activate (&rest _)
  "Activate all configuration files."
  (let* ((conf-dir (expand-file-name "conf.d/" user-config-directory))
         ;; (rx string-start (not ".") (*? anything) ".el" string-end)
         (conf-regex "\\`[^.][^z-a]*?\\.el\\'")
         (confs (directory-files-recursively conf-dir conf-regex)))
    (cl-loop for conf in confs
             do (load conf nil 'nomessage))))

(init-package-initialize)
(init-config-activate)

(provide 'init)
;;; init.el ends here