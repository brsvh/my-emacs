;;; early-init.el --- Early Init File -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2023 Burgess Chang

;; URL: https://github.com/brsvh/emacs.d
;; Keywords: internal
;; Package-Requires: ((emacs "29.1"))
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

;; This file is load before normal init file is loaded.

;;; Code:

(defmacro my-operating-system-p (os)
  "Return non-nil if OS corresponds to the current operating system.
Allowable values for OS (not quoted) are `macOS', `osx',
`windows', `linux', `unix'."
  (pcase os
    (`unix `(not (memq system-type '(ms-dos windows-nt cygwin))))
    ((or `macOS `osx) `(eq system-type 'darwin))
    (`linux `(not (memq system-type
                        '(darwin ms-dos windows-nt cygwin))))
    (`windows `(memq system-type '(ms-dos windows-nt cygwin)))))

(defmacro my-get-xdg-base-dir (concept)
  "Get the value of corresponds XDG Base Directory CONCEPT.
Allowable concepts (not quoted) are `cache', `config', `data' and
 `state'."
  (let* ((concepts '((cache . ("XDG_CACHE_HOME" . "~/.cache/"))
                     (config . ("XDG_CONFIG_HOME" . "~/.config/"))
                     (data . ("XDG_DATA_HOME" . "~/.local/share/"))
                     (state . ("XDG_STATE_HOME" . "~/.local/state/")))))
    `(let ((default-cons (cdr (assoc ',concept ',concepts))))
       (expand-file-name
        (or (getenv (car default-cons))
            (cdr default-cons))))))


(defconst my-cache-directory (if (my-operating-system-p linux)
                                 (expand-file-name
                                  "emacs/"
                                  (my-get-xdg-base-dir cache))
                               user-emacs-directory)
  "Directory beneath which additional volatile files are placed.")

(defconst my-config-directory user-emacs-directory
  "Directory beneath which additional config files are placed.")

(defconst my-data-directory (if (my-operating-system-p linux)
                                (expand-file-name
                                 "emacs/"
                                 (my-get-xdg-base-dir data))
                              user-emacs-directory)
  "Directory beneath which additional non-volatile files are placed.")

(defconst my-state-directory (if (my-operating-system-p linux)
                                 (expand-file-name
                                  "emacs/"
                                  (my-get-xdg-base-dir data))
                               user-emacs-directory)
  "Directory beneath which additional state files are placed.")

;; Redirect storage location for native compilation, it must be set
;; before all features are `require' if I want to set the follow
;; custom directory as the first priority.
(startup-redirect-eln-cache (concat my-cache-directory "eln-cache/"))

;; Add some essentia layout parameters of frame to preset values, and
;; ensure the minor modes corresponding to follow values are disabled.
(push (cons 'menu-bar-lines nil) default-frame-alist)
(push (cons 'tool-bar-lines nil) default-frame-alist)
(push (cons 'vertical-scroll-bars nil) default-frame-alist)
(push (cons 'horizontal-scroll-bars nil) default-frame-alist)
(setq-default menu-bar-mode nil tool-bar-mode nil scroll-bar-mode nil)