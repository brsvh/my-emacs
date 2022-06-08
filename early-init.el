;;; early-init.el --- Early Init File -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Burgess Chang

;; Author: Burgess Chang <bsc@brsvh.org>
;; License: GNU General Public License v3.0 or later
;; Package-Requires: ((emacs "28.1"))
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Version: 0.1

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This file is load before normal init file is loaded, we configure
;; some of necessary options that the init file depends on here.

;;; Code:

(let ((user (expand-file-name "lisp/user" user-emacs-directory)))
  (load (expand-file-name "user.el" user) nil 'nomessage)
  (load (expand-file-name "user-theme.el" user) nil 'nomessage))

;; Redirect storage location for native compilation, it must be set
;; before all features are `require' if we want to set the follow
;; custom directory as the first priority.
(setcar native-comp-eln-load-path
        (expand-file-name "eln-cache/" user-cache-directory))

;; Beginning at Emacs 27.0.50, package initialization occurs before
;; `user-init-file' is loaded, but after `early-init-file', so some of
;; necessary options of `package' we need to set in `early-init-file'.
(let ((elpa (expand-file-name "elpa/" user-cache-directory))
      (proto (if (gnutls-available-p) "https" "http")))
  (setq package-user-dir (format "%s%s/" elpa emacs-version)
	package-gnupghome-dir
	(expand-file-name "gnupg/" package-user-dir)
        ;; Inhibit package initialization, we manually handle it, also
        ;; before `user-init-file' is loaded.
        package-enable-at-startup nil
        ;; Enable native ompilation support of packages.
        package-native-compie t
        ;; Enable precompute activation actions to speed up package
        ;; activatation.
        package-quickstart t
        package-quickstart-file
        (expand-file-name "loaddefs.el" package-user-dir))
  ;; If any package directory exists, initialize the package system.
  (unless (bound-and-true-p package--activated)
    (catch 'package-dir-found
      (let ((dirs (cons package-user-dir package-directory-list)))
        (dolist (dir dirs)
	  (when (file-directory-p dir)
	    (dolist (subdir (directory-files dir))
	      (when (let ((subdir (expand-file-name subdir dir)))
                      (and (file-directory-p subdir)
                           (file-exists-p
                            (expand-file-name
                             (package--description-file subdir)
                             subdir))))
	        (throw 'package-dir-found t)))))))
    (package-activate-all))
  ;; By default, Emacs support download and install ELPA from
  ;; official archives site, aka GNU ELPA and Non-GNU ELPA.  But
  ;; these two archive repositories contain only a relatively small
  ;; part of the Emacs ecosystem, so we add Milkypostman’s ELPA for
  ;; convenient get more and more packages.
  (with-eval-after-load 'package
    (add-to-list 'package-archives
                 (cons "melpa" (concat proto "://melpa.org/packages/"))
                 'append)))

;; We save the Emacs Lisp packages that are not avaiable in ELPA
;; repositories in different directories under `user-emacs-directory'
;; as appropriate.  Make its can be found in Load Path before
;; initialization.
(let ((default-directory user-emacs-directory)
      (directories '("lisp/" "site-lisp/")))
  (normal-top-level-add-to-load-path directories)
  (dolist (directory directories)
    (let ((default-directory (expand-file-name directory)))
      (normal-top-level-add-subdirs-to-load-path))))

;; Inhibit Menu, Tool and Scroll Bars.
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(let* ((conf-dir (expand-file-name "themes/" user-emacs-directory))
       ;; (rx string-start (not ".") (*? anything) ".el" string-end)
       (conf-regex "\\`[^.][^z-a]*?\\.el\\'")
       (confs (directory-files conf-dir 'full conf-regex)))
  ;; Load theme configuration.
  (dolist (conf confs) 
    (load conf nil 'nomessage))
  ;; Some themes are require `cl-lib'.
  (eval-when-compile (require 'cl-lib))
  ;; Activate theme if it has been installed, otherwise deferred to
  ;; `after-init-hook' activate it.
  (if (member user-default-theme (custom-available-themes))
      (user-theme-activate user-default-theme)
    (add-hook 'after-init-hook
              #'(lambda () (user-theme-activate user-default-theme)))))

(provide 'early-init)
;;; early-init.el ends here