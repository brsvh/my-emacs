;;; early-init.el --- Early Init File -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Burgess Chang

;; Author: Burgess Chang <bsc@brsvh.org>
;; Keywords: internal
;; Version: 0.50.0

;; This file is part of emacs.d.

;; emacs.d is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; emacs.d is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with emacs.d. If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This file is load before normal init file is loaded, we configure
;; some of necessary options that the init file depends on here.

;;; Code:

(eval-when-compile (require 'package))

(defvar my-config-directory user-emacs-directory
  "Directory beneath which additional config files are placed.")

(defvar my-cache-directory (let ((linux '(berkeley-unix
                                          gnu
                                          gnu/kfreebsd
                                          gnu/linux))
                                 (xdg (getenv "XDG_CACHE_HOME")))
                             (if (and (memq system-type linux) xdg)
                                 (expand-file-name "emacs/" xdg)
                               user-emacs-directory))
  "Directory beneath which additional volatile files are placed.")

(defvar my-data-directory (let ((linux '(berkeley-unix
                                         gnu
                                         gnu/kfreebsd
                                         gnu/linux))
                                (xdg (getenv "XDG_DATA_HOME")))
                            (if (and (memq system-type linux) xdg)
                                (expand-file-name "emacs/" xdg)
                              user-emacs-directory))
  "Directory beneath which additional non-volatile files are placed.")

(defvar my-state-directory (let ((linux '(berkeley-unix
                                         gnu
                                         gnu/kfreebsd
                                         gnu/linux))
                                (xdg (getenv "XDG_STATE_HOME")))
                            (if (and (memq system-type linux) xdg)
                                (expand-file-name "emacs/" xdg)
                              user-emacs-directory))
  "Directory beneath which additional state files are placed.")

(defvar my-packages-file (expand-file-name "packages.el"
                                           my-config-directory)
  "Store here packages will be installed explicitly.")

(defun add-to-load-path (&rest subdirs)
  "Recursively add subdirectories SUBDIRS to `load-path'."
  (let ((default-directory
         (file-name-directory (macroexp-file-name))))
    (normal-top-level-add-to-load-path subdirs)
    (dolist (subdir subdirs)
      (let ((default-directory (expand-file-name subdir)))
        (normal-top-level-add-subdirs-to-load-path)))))

;; Defer garbage collection further back in the startup process.
(setq gc-cons-threshold most-positive-fixnum)
(add-hook 'after-init-hook
          #'(lambda ()
              (setq gc-cons-threshold
                    (car (get 'gc-cons-threshold 'standard-value)))))

(unless (or (daemonp) noninteractive)
  ;; Set the `file-name-handler' to `nil' since regexing is cpu
  ;; intensive.
  (let ((f file-name-handler-alist))
    (setq-default file-name-handler-alist nil)
    (add-hook 'after-init-hook
              #'(lambda ()
                  (delete-dups (append file-name-handler-alist f)))))
  ;; Premature redisplays can substantially affect startup times and
  ;; produce ugly flashes of unstyled Emacs.
  (setq-default inhibit-redisplay t
                inhibit-message t)
  (add-hook 'window-setup-hook
            (lambda ()
              (setq-default inhibit-redisplay nil
                            inhibit-message nil)
              (redisplay))))

;; Redirect storage location for native compilation, it must be set
;; before all features are `require' if we want to set the follow
;; custom directory as the first priority.
(startup-redirect-eln-cache (expand-file-name "eln-cache/"
                                              my-cache-directory))

;; We save the Emacs Lisp packages that are not avaiable in ELPA
;; repositories in different directories under `user-emacs-directory'
;; as appropriate.  Make its can be found in Load Path before
;; initialization.
(add-to-load-path "lisp/" "site-lisp/")

;; Beginning at Emacs 27.0.50, package initialization occurs before
;; `user-init-file' is loaded, but after `early-init-file', so some of
;; necessary options of `package' we need to set in `early-init-file'.
(setq package-user-dir (format "%s%s/"
                               (expand-file-name "elpa/"
                                                 my-cache-directory)
                               emacs-version)
      package-gnupghome-dir (expand-file-name "gnupg/"
                                              package-user-dir)
      ;; Enable native ompilation support of packages.
      package-native-compile t
      ;; Enable precompute activation actions to speed up package
      ;; activatation.
      package-quickstart t
      package-quickstart-file (expand-file-name "loaddefs.el"
                                                package-user-dir))

;; Save customization of easy customize to a separate file, rather
;; than append to `user-init-file'.
(setq custom-file (expand-file-name "custom.el" my-state-directory))
(when (file-exists-p custom-file) (load custom-file nil 'nomessage))

(with-eval-after-load "package"
  ;; By default, Emacs support download and install ELPA from official
  ;; archives site, aka GNU ELPA and Non-GNU ELPA.  But these two
  ;; archive repositories contain only a relatively small part of the
  ;; Emacs ecosystem, so we add Milkypostman’s ELPA for convenient get
  ;; more and more packages.
  (add-to-list 'package-archives
               (cons "melpa" (format "http%s://melpa.org/packages/"
                                     (if (gnutls-available-p)
                                         "s"
                                       "")))
               'append)

  ;; Make sure configuration macro feature is installed.
  (unless (package-installed-p 'use-package)
    (unless (memq 'use-package package-archive-contents)
      (package-refresh-contents))
    (package-install 'use-package)))

;; Add some essentia layout parameters of frame to preset values, and
;; ensure the minor modes corresponding to follow values are disabled.
(push (cons 'menu-bar-lines nil) default-frame-alist)
(push (cons 'tool-bar-lines nil) default-frame-alist)
(push (cons 'vertical-scroll-bars nil) default-frame-alist)
(push (cons 'horizontal-scroll-bars nil) default-frame-alist)
(setq-default menu-bar-mode nil tool-bar-mode nil scroll-bar-mode nil)

(provide 'early-init)
;;; early-init.el ends here
