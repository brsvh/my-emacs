;;; user.el --- user control -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Burgess Chang

;; Author: Burgess Chang <bsc@brsvh.org>
;; License: GNU General Public License v3.0 or later
;; Package-Requires: ((emacs "28.1"))
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Version: 0.1

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This file provides essential user control.

;;; Code:

(defgroup user nil
  "User control."
  :group 'emacs)

;;;###autoload
(defcustom user-config-directory user-emacs-directory
  "Directory beneath which additional configuration are placed."
  :group 'user
  :type 'string)

;;;###autoload
(defcustom user-data-directory
  (expand-file-name "emacs/" (or (getenv "XDG_DATA_HOME")
                                 "~/.local/share/"))
  "Directory beneath which additional non-volatile files are placed."
  :group 'user
  :type 'string)

;;;###autoload
(defcustom user-cache-directory
  (expand-file-name "emacs/" (or (getenv "XDG_CACHE_HOME")
                                 "~/.cache/"))
  "Directory beneath which additional volatile files are placed."
  :group 'user
  :type 'string)

;;;###autoload
(defcustom user-config-files nil
  "Store here configuration files added explicitly by user."
  :group 'user
  :type '(repeat string))

;;;###autoload
(defcustom user-package-files nil
  "Store here package files installed explicitly by user."
  :group 'user
  :type '(repeat string))

(provide 'user)
;;; user.el ends here