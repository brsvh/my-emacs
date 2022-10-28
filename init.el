;;; init.el --- Init File -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Burgess Chang

;; Author: Burgess Chang <bsc@brsvh.org>
;; Keywords: internal
;; Package-Requires: ((emacs "29.0.50") (org "9.5.5"))
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

;; This file is the first loaded file after Emacs is started.  We use
;; literate programming to make my Emacs configuration grow, so we
;; configure the automatic extract of the literate file.

;;; Code:

(require 'early-init (concat user-emacs-directory "early-init"))

(defvar my-literate-profile (concat user-emacs-directory "brsvh.org") ;
  "Burgess Chang's literate configuration profile.")

(defvar my-profile (concat my-data-directory "lisp/brsvh.el")
  "Burgess Chang's configuration profile.")

(defun my-profile-publish (file target-file)
  "Try to export code from the literate FILE to TARGET-FILE.

Optional argument TARGET-FILE can be used to specify a default
export file for all source blocks.

Return a list whose CAR is the tangled file name, and CDR is t if
FILE is tangled, otherwise nil."
  (unless (fboundp 'org-babel-tangle-file)
    (autoload 'org-babel-tangle-file "ob-tangle"))
  (eval-when-compile
    (declare-function org-babel-tangle-file "ob-tangle"))
  (if (or (not (file-exists-p file))
          (file-newer-than-file-p my-literate-profile my-profile))
      (cons (car (org-babel-tangle-file file target-file)) t)
    (cons target-file nil)))

(load (car (my-profile-publish my-literate-profile my-profile)) nil t)

(provide 'init)
;;; init.el ends here
