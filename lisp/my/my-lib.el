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
  (if (not (my-os-is windows))
      (my-path* (my-xdg-base-dir-for cache) "emacs/")
    user-emacs-directory)
  "Directory beneath which additional volatile files are placed."
  :group 'my
  :type 'directory)

;;;###autoload
(defcustom my-config-directory
  (if (not (my-os-is windows))
      (my-path* (my-xdg-base-dir-for config) "emacs/")
    user-emacs-directory)
  "Directory beneath which additional config files are placed."
  :type 'directory
  :group 'my)

;;;###autoload
(defcustom my-data-directory
  (if (not (my-os-is windows))
      (my-path* (my-xdg-base-dir-for data) "emacs/")
    user-emacs-directory)
  "Directory beneath which additional non-volatile files are placed."
  :group 'my
  :type 'directory)

;;;###autoload
(defcustom my-state-directory
  (if (not (my-os-is windows))
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
(defgroup my-fonts nil
  "Fonts for my Emacs configuraiton."
  :group 'my)

;;;###autoload
(defcustom my-font-name ""
  "Name of my default font."
  :group 'my-fonts
  :type 'string)

;;;###autoload
(defcustom my-font-size ""
  "Name of my default font."
  :group 'my-fonts
  :type 'string)

;;;###autoload
(defcustom my-latin-font-name ""
  "Name of my latin font."
  :group 'my-fonts
  :type 'string)

;;;###autoload
(defcustom my-chinese-font-name ""
  "Name of my chinese font."
  :group 'my-fonts
  :type 'string)

;;;###autoload
(defcustom my-symbol-font-name ""
  "Name of my symbol font."
  :group 'my-fonts
  :type 'string)

(provide 'my-lib)
;;; my-lib.el ends here
