;;; my-core.el --- Heart of My Emacs -*- lexical-binding: t -*-

;; Copyright (C) 2022-2024 Burgess Chang

;; Author: Burgess Chang <bsc@brsvh.org>
;; Keywords: local
;; Package-Requires: ((emacs "29.1") (my-lib "0.1.0") (setup "1.4.0"))
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

(require 'my-lib)
(require 'setup)



;;;
;; `setup` keywords:

(setup-define :eval-when
  (lambda (timing &rest body)
    `(cl-eval-when ,timing ,@body))
  :documentation "Control when BODY is evaluated by TIMING.
Usage see `cl-eval-when'."
  :debug '(sexp body)
  :indent 1)

(setup-define :init
  (lambda (&rest body)
    (macroexp-progn body))
  :documentation "BODY to run before NAME has been loaded."
  :debug '(form)
  :after-loaded nil
  :indent 1)

(setup-define :after
  (lambda (&rest features)
    (let ((body `(require ',(setup-get 'feature))))
      (dolist (feature (nreverse features))
        (setq body `(with-eval-after-load ',feature ,body)))
      body))
  :documentation "Load the current feature after FEATURES.
See https://www.emacswiki.org/emacs/SetupEl#h5o-10."
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


;;;
;; Keymaps:

(defvar my-ctl-c-map (make-keymap)
  "Default keymap for my commands.")
(defvaralias 'ctl-c-map 'my-ctl-c-map)

(defvar my-ctl-c-v-map (make-keymap)
  "Default keymap for version control commands.")
(defvaralias 'ctl-c-v-map 'my-ctl-c-v-map)

(defvar my-ctl-c-v-g-map (make-keymap)
  "Default keymap for version control (Git) commands.")
(defvaralias 'ctl-c-v-g-map 'my-ctl-c-v-g-map)

(setup my-maps
  (:with-map ctl-c-map
    (:keymap-set "v" ctl-c-v-map)
    (:with-map ctlc-c-v-map
      (:keymap-set "g" ctl-c-v-g-map)))
  (:keymap-set-into gloabl-map "C-c" ctl-c-map))

(provide 'my-core)
;;; my-core.el ends here
