;;; define-keys.el --- Define Keys -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Burgess Chang

;; Author: Burgess Chang <bsc@brsvh.org>
;; License: GNU General Public License v3.0 or later
;; Package-Requires: ((general "0.1"))
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Version: 0.1

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This file provides simple wrap for `general'.

;;; Code:

;;;###autoload
(defmacro define-keys (&rest args)
  "Define key with a variable number of positional ARGS."
  `(general-define-key
    ,@args))

(provide 'define-keys)
;;; define-keys.el ends here