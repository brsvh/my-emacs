;;; my-core.el --- Heart of My Emacs -*- lexical-binding: t -*-

;; Copyright (C) 2022-2024 Burgess Chang

;; Author: Burgess Chang <bsc@brsvh.org>
;; Keywords: local
;; Package-Requires: ((emacs "29.1") (my-lib "0.1.0") (on "0.1.0") (setup "1.4.0"))
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

;; The heart of my Emacs configuraiton, setup the prerequisites for
;; start the play and arrange the various instruments.

;;; Code:

(require 'cl-lib)
(require 'dash)
(require 'my-lib)
(require 'on)
(require 'setup)



;;;
;; `setup` keywords:

(cl-eval-when (compile eval load)

  (setup-define :after
    (lambda (feature &rest body)
      `(with-eval-after-load ',feature ,@body))
    :documentation "Eval BODY after FEATURE."
    :after-loaded t
    :indent 1)

  (setup-define :autoload
    (lambda (func)
      (let ((fn (if (memq (car-safe func) '(quote function))
                    (cadr func)
                  func)))
        `(unless (fboundp (quote ,fn))
           (autoload (function ,fn)
             ,(symbol-name (setup-get 'feature))
             nil
             t))))
    :documentation "Autoload FUNC if not already bound."
    :repeatable t
    :signature '(FUNC ...))

  (setup-define :eval-when
    (lambda (timing &rest body)
      `(cl-eval-when ,timing ,@body))
    :documentation "Control when BODY is evaluated by TIMING.
Usage see `cl-eval-when'."
    :debug '(sexp body)
    :indent 1)

  (setup-define :face
    (lambda (face spec)
      `(custom-set-faces (quote (,face ,spec))))
    :documentation "Customize FACE to SPEC."
    :signature '(face spec ...)
    :debug '(setup)
    :repeatable t
    :after-loaded t)

  (setup-define :first-buffer
    (lambda (function)
      `(add-hook 'on-first-buffer-hook ,function))
    :documentation "Add FUNCTION to `on-first-buffer-hook'."
    :ensure '(func)
    :repeatable t)

  (setup-define :first-file
    (lambda (function)
      `(add-hook 'on-first-file-hook ,function))
    :documentation "Add FUNCTION to `on-first-file-hook'."
    :ensure '(func)
    :repeatable t)

  (setup-define :first-input
    (lambda (function)
      `(add-hook 'on-first-input-hook ,function))
    :documentation "Add FUNCTION to `on-first-input-hook'."
    :ensure '(func)
    :repeatable t)

  (setup-define :first-ui
    (lambda (function)
      `(add-hook 'on-init-ui-hook ,function))
    :documentation "Add FUNCTION to `on-init-ui-hook'."
    :ensure '(func)
    :repeatable t)

  (setup-define :gui
    (lambda (&rest body)
      `(when (display-graphic-p)
         ,@body))
    :documentation "Evaluate body when `display-graphic-p' is non-nil."
    :debug '(form))

  (setup-define :init
    (lambda (&rest body)
      (macroexp-progn body))
    :documentation "BODY to run before NAME has been loaded."
    :debug '(form)
    :after-loaded nil
    :indent 1)

  (setup-define :keymap-set
    (lambda (key definition)
      `(keymap-set ,(setup-get 'map) ,key ,definition))
    :documentation "Set KEY to DEFINITION.
See `keymap-set'."
    :debug '(key sexp)
    :repeatable t)

  (setup-define :keymap-set-into
    (lambda (feature-or-map &rest body)
      (if (string-match-p "-map\\'" (symbol-name feature-or-map))
          (progn
            `(:with-map ,feature-or-map (:keymap-set ,@body)))
        `(:with-feature ,feature-or-map (:keymap-set ,@body))))
    :documentation "Set keys to definition into FEATURE-OR-MAP."
    :debug '(sexp &rest key sexp))

  (setup-define :keymap-unset
    (lambda (key remove)
      `(keymap-set ,(setup-get 'map) ,key ,remove))
    :documentation "Unset or REMOVE definition of KEY.
See `keymap-unset'."
    :debug '(key boolean)
    :repeatable t)

  (setup-define :keymap-unset-into
    (lambda (feature-or-map &rest body)
      (if (string-match-p "-map\\'" (symbol-name feature-or-map))
          (progn
            `(:with-map ,feature-or-map (:keymap-unset ,@body)))
        `(:with-feature ,feature-or-map (:keymap-unset ,@body))))
    :documentation "Set keys to definition into FEATURE-OR-MAP."
    :debug '(sexp &rest key boolean))

  (setup-define :quit
    #'setup-quit
    :documentation "Unconditionally abort the evaluation.")

  (setup-define :require-after
    (lambda (&rest features)
      (let ((body `(require ',(setup-get 'feature))))
        (dolist (feature (nreverse features))
          (setq body `(with-eval-after-load ',feature ,body)))
        body))
    :documentation "Load the current feature after FEATURES.
See https://www.emacswiki.org/emacs/SetupEl#h5o-10."
    :indent 1)

  (setup-define :set
    (setup-make-setter
     (lambda (symbol)
       `(funcall #'symbol-value ',symbol))
     (lambda (symbol value)
       `(funcall #'set ',symbol ,value)))
    :documentation "Set SYMBOL's value to VALUE.
These forms are supported:

(append VAR)   Assuming VAR designates a list, add VAL as its last
               element, unless it is already member of the list.

(prepend VAR)  Assuming VAR designates a list, add VAL to the beginning,
               unless it is already member of the list.

(remove VAR)   Assuming VAR designates a list, remove all instances of
               VAL.

(append* VAR)  Assuming VAR designates a list, add each element of VAL
               to the end of VAR, keeping their order, unless it is
               already a member of the list.

(prepend* VAR) Assuming VAR designates a list, add each element of VAL
               to the start of VAR, keeping their order, unless it is
               already a member of the list.

(remove* VAR)  Assuming VAR designates a list, remove all instances of
               each element of VAL."
    :debug '(sexp form)
    :repeatable t)

  (setup-define :set-default
    (setup-make-setter
     (lambda (symbol)
       `(funcall #'symbol-value ',symbol))
     (lambda (symbol value)
       `(funcall #'set-default ',symbol ,value)))
    :documentation "Set SYMBOL's default value to VALUE.
These forms are supported:

(append VAR)   Assuming VAR designates a list, add VAL as its last
               element, unless it is already member of the list.

(prepend VAR)  Assuming VAR designates a list, add VAL to the beginning,
               unless it is already member of the list.

(remove VAR)   Assuming VAR designates a list, remove all instances of
               VAL.

(append* VAR)  Assuming VAR designates a list, add each element of VAL
               to the end of VAR, keeping their order, unless it is
               already a member of the list.

(prepend* VAR) Assuming VAR designates a list, add each element of VAL
               to the start of VAR, keeping their order, unless it is
               already a member of the list.

(remove* VAR)  Assuming VAR designates a list, remove all instances of
               each element of VAL."
    :debug '(sexp form)
    :repeatable t)

  (setup-define :snoc
    (lambda (symbol elem &rest elements)
      `(:set ,symbol (funcall #'-snoc ,symbol ,elem ,@elements)))
    :documentation "Append ELEM and ELEMENTS to the end of SYMBOL."
    :debug '(sexp sexp form))

  (setup-define :snoc-local
    (lambda (symbol elem &rest elements)
      `(add-hook ',(setup-get 'hook)
                 (lambda ()
                   (setq-local ,symbol (funcall #'-snoc
                                                ,symbol
                                                ,elem
                                                ,@elements)))))
    :documentation "Append ELEM and ELEMENTS to the end of SYMBOL."
    :debug '(sexp sexp form))

  (setup-define :tui
    (lambda (&rest body)
      `(unless (display-graphic-p)
         ,@body))
    :documentation "Evaluate body when `display-graphic-p' is nil."
    :debug '(form))
  
  )



;;;
;; Keymaps:

(defvar ctl-c-map (make-keymap)
  "Default keymap for my commands.")

(defvar ctl-c-v-map (make-keymap)
  "Default keymap for version control commands.")

(defvar ctl-c-v-g-map (make-keymap)
  "Default keymap for version control (Git) commands.")

(setup my-maps
  (:with-map ctl-c-map
    (:keymap-set "v" ctl-c-v-map))
  (:with-map ctl-c-v-map
    (:keymap-set "g" ctl-c-v-g-map))
  (:keymap-set-into global-map "C-c" ctl-c-map))



;;;
;; Third-Party libraries:

(setup svg-lib
  (:set-default
   svg-lib-icons-dir (my-data-path "svg-icons/")))

(setup transient
  (:set-default
   transient-history-file (my-state-path "transient/history.el")
   transient-levels-file (my-state-path "transient/levels.el")
   transient-values-file (my-state-path "transient/values.el")))

(provide 'my-core)
;;; my-core.el ends here
