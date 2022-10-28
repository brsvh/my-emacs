;;; early-init.el --- Early Init File -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Burgess Chang

;; Author: Burgess Chang <bsc@brsvh.org>
;; Keywords: internal
;; Package-Requires: ((emacs "29.0.50"))
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

;; This file is load before normal init file is loaded, I configure
;; `package' and `comp' options that the init file and my actual
;; profile depends on here.

;;; Code:

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

;; Redirect storage location for native compilation, it must be set
;; before all features are `require' if I want to set the follow
;; custom directory as the first priority.
(startup-redirect-eln-cache (concat my-cache-directory "eln-cache/"))

(eval-when-compile (require 'package))

;; Beginning at Emacs 27.0.50, package initialization occurs before
;; `user-init-file' is loaded, but after `early-init-file', so some of
;; necessary options of `package' I need to set in `early-init-file'.
(setq package-user-dir (format "%s%s/"
                               (concat my-cache-directory "elpa/")
                               emacs-version)
      package-gnupghome-dir (concat package-user-dir "gnupg/")
      ;; Enable native ompilation support of packages.
      package-native-compile t
      ;; Enable precompute activation actions to speed up package
      ;; activatation.
      package-quickstart t
      package-quickstart-file (concat package-user-dir "loaddefs.el"))

;; Add some essentia layout parameters of frame to preset values, and
;; ensure the minor modes corresponding to follow values are disabled.
(push (cons 'menu-bar-lines nil) default-frame-alist)
(push (cons 'tool-bar-lines nil) default-frame-alist)
(push (cons 'vertical-scroll-bars nil) default-frame-alist)
(push (cons 'horizontal-scroll-bars nil) default-frame-alist)
(setq-default menu-bar-mode nil tool-bar-mode nil scroll-bar-mode nil)

(provide 'early-init)
;;; early-init.el ends here
