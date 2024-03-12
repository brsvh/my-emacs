;;; my-lib.el --- My Emacs Lisp extensions  -*- lexical-binding: t -*-

;; Copyright (C) 2022-2024 Burgess Chang

;; Author: Burgess Chang <bsc@brsvh.org>
;; Keywords: extensions
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

;; These are extensions that provide additional features to support my
;; personal Emacs configuration.

;;; Code:

;;;###autoload
(defgroup my nil
  "Customize my Emacs Configuration."
  :prefix "my-"
  :group 'emacs
  :link '(url-link :tag "GitHub" "https://github.com/brsvh/my-emacs"))

;;;###autoload
(defgroup my-hooks nil
  "Hooks for my Emacs Configuration."
  :group 'my
  :link '(url-link :tag "GitHub" "https://github.com/brsvh/my-emacs"))

;;;###autoload
(defvar my-true t
  "A symbol always mean true.")

;;;###autoload
(defmacro my-os-is (os)
  "Return non-nil if OS corresponds to the current operating system.
Allowable values for OS (not quoted) are `macOS', `osx', `windows',
`linux', `unix'."
  (pcase os
    (`unix `(not (memq system-type '(ms-dos windows-nt cygwin))))
    ((or `macOS `osx) `(eq system-type 'darwin))
    (`linux `(not (memq system-type
                        '(darwin ms-dos windows-nt cygwin))))
    (`windows `(memq system-type '(ms-dos windows-nt cygwin)))))

;;;###autoload
(defmacro my-xdg-base-dir-for (concept)
  "Get the value of corresponds XDG Base Directory CONCEPT.
Allowable concepts (not quoted) are `cache', `config', `data' and
 `state'."
  (let* ((concepts '((cache . ("XDG_CACHE_HOME" . "~/.cache/"))
                     (config . ("XDG_CONFIG_HOME" . "~/.config/"))
                     (data . ("XDG_DATA_HOME" . "~/.local/share/"))
                     (state . ("XDG_STATE_HOME" . "~/.local/state/")))))
    `(let ((default-cons (cdr (assoc ',concept ',concepts))))
       (expand-file-name (or (getenv (car default-cons))
                             (cdr default-cons))))))

;;;###autoload
(defun my-path (&rest segments)
  "Join SEGMENTS to a path."
  (let (file-name-handler-alist path)
    (setq path (expand-file-name (if (cdr segments)
                                     (apply #'file-name-concat segments)
                                   (car segments))))
    (if (file-name-absolute-p (car segments))
        path
      (file-relative-name path))))

;;;###autoload
(defun my-path= (a b)
  "Return t if A and B are references to same path."
  (equal
   (file-truename (directory-file-name (my-path a)))
   (file-truename (directory-file-name (my-path b)))))

;;;###autoload
(defun my-dirname (path)
  "Return the parent directory to PATH."
  (let ((s  (file-name-directory (directory-file-name (my-path path)))))
    (unless (my-path= path s)
      (if (file-name-absolute-p path)
          (directory-file-name s)
        (file-relative-name s)))))

;;;###autoload
(defun my-path* (&rest segments)
  "Join SEGMENTS to a path, ensure it is exists."
  (let ((path (apply #'my-path segments)) dir)
    (if (and (my-os-is linux) (string-suffix-p "/" path))
        (setq dir path)
      (setq dir (my-dirname path)))
    (make-directory dir 'parents)
    path))

;;;###autoload
(defcustom my-cache-directory
  (if (my-os-is linux)
      (my-path* (my-xdg-base-dir-for cache) "emacs/")
    user-emacs-directory)
  "Directory beneath which additional volatile files are placed."
  :group 'my
  :type 'directory)

;;;###autoload
(defcustom my-config-directory user-emacs-directory
  "Directory beneath which additional config files are placed."
  :type 'directory
  :group 'my)

;;;###autoload
(defcustom my-data-directory
  (if (my-os-is linux)
      (my-path* (my-xdg-base-dir-for data) "emacs/")
    user-emacs-directory)
  "Directory beneath which additional non-volatile files are placed."
  :group 'my
  :type 'directory)

;;;###autoload
(defcustom my-state-directory
  (if (my-os-is linux)
      (my-path* (my-xdg-base-dir-for state) "emacs/")
    user-emacs-directory)
  "Directory beneath which additional state files are placed."
  :group 'my
  :type 'directory)

;;;###autoload
(defun my-cache-path (&rest segments)
  "Join SEGMENTS to `my-cache-directory'."
  (apply #'my-path (append (list my-cache-directory) segments)))

;;;###autoload
(defun my-cache-path* (&rest segments)
  "Join SEGMENTS to `my-cache-directory', ensure it is exists."
  (apply #'my-path* (append (list my-cache-directory) segments)))

;;;###autoload
(defun my-config-path (&rest segments)
  "Join SEGMENTS to `my-config-directory'."
  (apply #'my-path (append (list my-config-directory) segments)))

;;;###autoload
(defun my-config-path* (&rest segments)
  "Join SEGMENTS to `my-config-directory', ensure it is exists."
  (apply #'my-path* (append (list my-config-directory) segments)))

;;;###autoload
(defun my-data-path (&rest segments)
  "Join SEGMENTS to `my-data-directory'."
  (apply #'my-path (append (list my-data-directory) segments)))

;;;###autoload
(defun my-data-path* (&rest segments)
  "Join SEGMENTS to `my-data-directory', ensure it is exists."
  (apply #'my-path* (append (list my-data-directory) segments)))

;;;###autoload
(defun my-state-path (&rest segments)
  "Join SEGMENTS to `my-state-directory'."
  (apply #'my-path (append (list my-state-directory) segments)))

;;;###autoload
(defun my-state-path* (&rest segments)
  "Join SEGMENTS to `my-state-directory', ensure it is exists."
  (apply #'my-path* (append (list my-state-directory) segments)))

;;;###autoload
(defcustom my-custom-file
  (my-state-path "custom.el")
  "Location of my `custom-file'."
  :group 'my
  :type 'file)

;;;###autoload
(defcustom my-eln-cache-directory
  (my-cache-path "eln-cache/")
  "Directory beneath which my natively-compiled files."
  :group 'my
  :type 'directory)

;;;###autoload
(defcustom my-elpa-directory
  (my-data-path (format "elpa/%s/" emacs-version))
  "Directory beneath which my Emacs Lisp packages."
  :group 'my
  :type 'directory)

;;;###autoload
(defcustom my-authinfo-file (my-data-path "authinfo")
  "My autoinfo file."
  :group 'my
  :type 'file)

;;;###autoload
(defcustom my-authinfo-gpg-file (my-data-path "authinfo.gpg")
  "My autoinfo.gpg file."
  :group 'my
  :type 'file)

(defvar my-ctl-c-map (make-keymap)
  "Default keymap for C-c commands.")

(defvar my-ctl-c-5-map (make-keymap)
  "Default keymap for C-c 5 commands.")

(defvar my-ctl-c-a-map (make-keymap)
  "Default keymap for C-c a commands.")

(defvar my-ctl-c-e-map (make-keymap)
  "Default keymap for C-c e commands.")

(defvar my-ctl-c-f-map (make-keymap)
  "Default keymap for C-c f commands.")

(defvar my-ctl-c-m-map (make-keymap)
  "Default keymap for C-c m commands.")

(defvar my-ctl-c-n-map (make-keymap)
  "Default keymap for C-c n commands.")

(defvar my-ctl-c-v-map (make-keymap)
  "Default keymap for C-c v commands.")

(defvar my-ctl-c-v-g-map (make-keymap)
  "Default keymap for C-c v g commands.")

;;;###autoload
(defcustom my-light-theme 'modus-operandi-tinted
  "Default light theme."
  :group 'my
  :type 'symbol)

;;;###autoload
(defcustom my-dark-theme 'modus-vivendi-tinted
  "Default dark theme."
  :group 'my
  :type 'symbol)

;;;###autoload
(defcustom my-theme my-light-theme
  "Default theme."
  :group 'my
  :type 'symbol)

;;;###autoload
(defun my-select-proper-theme (&rest _)
  "Set `my-theme' to proper variant base on window system type."
  (if (display-graphic-p)
      (setq my-theme my-light-theme)
    (setq my-theme my-dark-theme)))

;;;###autoload
(defun my-load-theme (theme)
  "Load THEME."
  (when (eq theme 'default)
    (setq theme nil))
  (unless (eq theme (car custom-enabled-themes))
    (mapc #'disable-theme custom-enabled-themes)
    (when theme
      (if (custom-theme-p theme)
          (enable-theme theme)
        (load-theme theme :no-confirm)))))

;;;###autoload
(defun my-theme-setup (&rest _)
  "Setup `my-theme'."
  (my-select-proper-theme)
  (my-load-theme my-theme))

;;;###autoload
(defun my/toggle-theme ()
  "Toggle themes."
  (interactive)
  (cond ((member my-light-theme custom-enabled-themes)
         (my-load-theme my-dark-theme))
        ((member my-light-theme custom-enabled-themes)
         (my-load-theme my-dark-theme))
    (t (my-theme-setup))))

(defcustom my-gui-init-hook nil
  "Hooks run for gui frame (with window system) initialization."
  :group 'my-hooks
  :type 'hook)

;;;###autoload
(defun my-gui-init-actions (&rest _)
  "The actions will be performed when GUI initialize."
  (run-hooks 'my-gui-init-hook))

;;;###autoload
(defcustom my-nogui-init-hook nil
  "Hooks run for nogui frame (without window system) initialization."
  :group 'my-hooks
  :type 'hook)

;;;###autoload
(defun my-nogui-init-actions (&rest _)
  "The actions will be performed when GUI initialize."
  (run-hooks 'my-nogui-init-hook))

;;;###autoload
(defcustom my-init-hook nil
  "Hooks run after `user-init-file' has been loaded."
  :group 'my-hooks
  :type 'hook)

;;;###autoload
(defun my-init-actions (&rest _)
  "The actions will be performed when `user-init-file' loaded."
  (run-hooks 'my-init-hook))

;;;###autoload
(defcustom my-local-file (my-config-path "local.el")
  "My local file."
  :group 'my
  :type 'file)

;;;###autoload
(defun my-load-local-file (&rest _)
  "Load `my-local-file' if it is exists."
  (when (file-exists-p my-local-file)
    (load my-local-file nil 'nomessage)))

;;;###autoload
(defun my/server-start ()
    "Allow this Emacs process to be a server for client processes."
    (interactive)
    (eval-when-compile (require 'server))
  (unless (server-running-p) (server-start)))

;;;###autoload
(progn
  (add-hook 'after-make-frame-functions #'my-gui-init-actions -100)
  (add-hook 'tty-setup-hook #'my-nogui-init-actions -100)
  (add-hook 'after-init-hook #'my-init-actions -100))

(provide 'my-lib)
;;; my-lib.el ends here
