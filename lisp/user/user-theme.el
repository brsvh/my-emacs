;;; user-theme.el --- user theme control -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Burgess Chang

;; Author: Burgess Chang <bsc@brsvh.org>
;; License: GNU General Public License v3.0 or later
;; Package-Requires: ((emacs "28.1") (user "0.1"))
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Version: 0.1

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This file provides activation and switch of user themes.

;;; Code:

(defgroup user-theme nil
  "User control."
  :group 'user)

;;;###autoload
(defcustom user-default-theme nil
  "A symbol representing the Emacs theme to load at startup."
  :group 'user-theme
  :type 'symbol)

;;;###autoload
(defcustom user-light-theme nil
  "A symbol representing the prefered light Emacs theme."
  :group 'user-theme
  :type 'symbol)

;;;###autoload
(defcustom user-dark-theme nil
  "A symbol representing the prefered dark Emacs theme."
  :group 'user-theme
  :type 'symbol)

;;;###autoload
(defcustom user-theme-load-hook nil
  "Normal hook run after theme switch."
  :group 'user-theme
  :type 'hook)

;;;###autoload
(defun user-theme-activate (theme)
  "Activate THEME."
  (unless (eq theme (car custom-enabled-themes))
    (mapc #'disable-theme custom-enabled-themes)
    (if (custom-theme-p theme)
        (enable-theme theme)
      (load-theme theme 'no-confirm))
    (run-hooks 'user-theme-load-hook)))

;;;###autoload
(defun light ()
  "Activate the light theme."
  (interactive)
  (user-theme-activate user-light-theme))

;;;###autoload
(defun dark ()
  "Activate the dark theme."
  (interactive)
  (user-theme-activate user-dark-theme))

(provide 'user-theme)
;;; user-theme.el ends here
