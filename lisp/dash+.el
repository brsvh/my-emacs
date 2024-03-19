;;; dash+.el --- Extra extensions for `dash' -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2024 Burgess Chang

;; Author: Burgess Chang <bsc@brsvh.org>
;; Keywords: extensions
;; Package-Requires: ((emacs "29.1") (dash "2.19.1"))
;; URL: https://github.com/brsvh/my-emacs
;; Version: 0.1.0

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

;; These are extensions that provide additional features of `dash'.

;;; Code:

(require 'dash)

;;;###autoload
(defmacro -concatq (seq &rest sequences)
  "Concatenate SEQ with SEQUENCES in place."
  (declare (indent nil))
  `(setq ,seq (-concat ,seq ,@sequences)))

;;;###autoload
(defmacro -concatq-local (seq &rest sequences)
  "Make SEQ buffer-local and concatenate it with SEQUENCES in place."
  (declare (indent nil))
  `(setq-local ,seq (-concat ,seq ,@sequences)))

;;;###autoload
(defmacro -snocq (list elem &rest elements)
  "Append ELEM and ELEMENTS to the end of the LIST in place."
  (declare (indent nil))
  `(setq ,list (-snoc ,list ,elem ,@elements)))

;;;###autoload
(defmacro -snocq-local (list elem &rest elements)
  "Append ELEM and ELEMENTS to the end of the LIST in place."
  (declare (indent nil))
  `(setq-local ,list (-snoc ,list ,elem ,@elements)))

(provide 'dash+)
;;; dash+.el ends here