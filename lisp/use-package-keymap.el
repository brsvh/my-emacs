;;; use-package-keymap.el --- Support for :keymap-set and :keymap-unset keyword -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2024 Burgess Chang

;; Author: Burgess Chang <bsc@brsvh.org>
;; Keywords: internal
;; Package-Requires: ((emacs "29.1"))
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

;; Provides support for the :keymap-set and :keymap-unset keywords,
;; which are made available by default by requiring `use-package'.
;;
;; Example:
;;  (use-package foo
;;    :commands (foo bar)
;;    :keymap-set
;;    ("C-c f" . foo)      ;; => (keymap-set global-map "C-c f" 'foo)
;;    (:foo-mode-map
;;     ("C-c f" . foo)     ;; => (keymap-set foo-mode-map "C-c f" 'foo)
;;     ("C-c b" . bar)     ;; => (keymap-set foo-mode-map "C-c b" 'bar)
;;     :bar-mode-map
;;     (("C-c f" . foo)    ;; => (keymap-set bar-mode-map "C-c f" 'foo)
;;      ("C-c b" . bar)))) ;; => (keymap-set bar-mode-map "C-c b" 'bar)
;;
;;  (use-package foo
;;    :keymap-unset
;;    ("C-c f" . t)        ;; => (keymap-unset global-map "C-c f" t)
;;    ("C-c b" . nil)      ;; => (keymap-unset global-map "C-c b" nil)
;;    (:foo-mode-map
;;     ("C-c f" . t)       ;; => (keymap-unset foo-mode-map "C-c f" t)
;;     ("C-c b" . nil)))   ;; => (keymap-unset foo-mode-map "C-c b" nil)

;;; Code:

(require 'cl-lib)
(require 'use-package)

;;;###autoload
(defun use-package-normalize/:keymap-set (name keyword args &optional map)
  "`use-package' normalizer of :keymap-set keyword."
  (or map (setq map 'global-map))
  (let ((args* args) (ctx-map map) result)
    (while args*
      (cond
       ((and (consp (car args*))
             (or (stringp (caar args*))
                 (vectorp (caar args*)))
             (or (use-package-recognize-function (cdar args*)
                                                 t
                                                 #'stringp)))
        (setq result (nconc result (list (list map
                                               (caar args*)
                                               `',(cdar args*))))
              args* (cdr args*)))
       ((keywordp (car args*))
        (setq ctx-map (intern (substring (symbol-name (car args*))
                                         1))
              result (nconc result
                            (use-package-normalize/:keymap-set
                             name keyword (cdr args*) ctx-map))
              args* nil))
       ((listp (car args*))
        (setq result (nconc result
                            (use-package-normalize/:keymap-set
                             name keyword (car args*) ctx-map))
              args* (cdr args*)))
       (t
        (use-package-error
         (concat (symbol-name name)
                 " wants arguments acceptable to the `keymap-set'"
                 " functions, or a list of such values")))))
    result))

;;;###autoload
(defun use-package-handler/:keymap-set (name keyword args rest state)
  "`use-package' handler of :keymap-set keyword."
  (use-package-concat
   (mapcar
    #'(lambda (keybinding) `(keymap-set ,@keybinding))
    args)
   (use-package-process-keywords name rest state)))

;;;###autoload
(defun use-package-normalize/:keymap-unset (name keyword args &optional map)
  "`use-package' normalizer of :keymap-unset keyword."
  (or map (setq map 'global-map))
  (let ((args* args) (ctx-map map) result)
    (while args*
      (cond
       ((and (consp (car args*))
             (or (stringp (caar args*))
                 (vectorp (caar args*)))
             (or (use-package-recognize-function (cdar args*)
                                                 t
                                                 #'stringp)))
        (setq result (nconc result (list (list map
                                               (caar args*)
                                               (cdar args*))))
              args* (cdr args*)))
       ((keywordp (car args*))
        (setq ctx-map (intern (substring (symbol-name (car args*))
                                         1))
              result (nconc result
                            (use-package-normalize/:keymap-unset
                             name keyword (cdr args*) ctx-map))
              args* nil))
       ((listp (car args*))
        (setq result (nconc result
                            (use-package-normalize/:keymap-unset
                             name keyword (car args*) ctx-map))
              args* (cdr args*)))
       (t
        (use-package-error
         (concat (symbol-name name)
                 " wants arguments acceptable to the `keymap-unset'"
                 " functions, or a list of such values")))))
    result))

;;;###autoload
(defun use-package-handler/:keymap-unset (name keyword args rest state)
  "`use-package' handler of :keymap-unset keyword."
  (use-package-concat
   (mapcar
    #'(lambda (keybinding) `(keymap-unset ,@keybinding))
    args)
   (use-package-process-keywords name rest state)))

;;;###autoload
(defun use-package-keymap-add-keyword (keyword position)
  "Insert KEYWORD to `use-package-keywords' after POSITION."
  (unless (member keyword use-package-keywords)
    (unless (and (keywordp keyword)
                 (keywordp position))
      (error "KEYWORD and POSITION must be keyword"))
    (setq use-package-keywords
          (let* ((pos (cl-position position use-package-keywords))
                 (head (cl-subseq use-package-keywords 0 (+ 1 pos)))
                 (tail (nthcdr (+ 1 pos) use-package-keywords)))
            (append head (list keyword) tail)))))

(use-package-keymap-add-keyword :keymap-set :custom-face)
(use-package-keymap-add-keyword :keymap-unset :keymap-set)

(provide 'use-package-keymap)
;;; use-package-keymap-set.el ends here
