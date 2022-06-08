;;; inplace.el --- inplace extensions -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Burgess Chang

;; Author: Burgess Chang <bsc@brsvh.org>
;; License: GNU General Public License v3.0 or later
;; Package-Requires: ((emacs "28.1"))
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Version: 0.1

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This file provides in place features.

;;; Code:

;;;###autoload
(defmacro appendq (symbol &rest sequences)
  "Set SYMBOL to the result of concatenate all SEQUENCES and itself."
  `(setq ,symbol (append ,symbol ,@sequences)))

;;;###autoload
(defun prepend (symbol &rest sequences)
  "Concatenate SEQUENCES to the beginning of SYMBOL."
  (let ((symbol (indirect-variable symbol)) result)
    (dolist (sequence sequences)
      (setq result (append result sequence)))
    (append result symbol)))

;;;###autoload
(defmacro prependq (symbol &rest sequences)
  "Set the value of SYMBOL to concatenate with SEQUENCES at front."
  `(setq ,symbol (prepend ,symbol ,@sequences)))

(provide 'inplace)
;;; inplace.el ends here
