;;; init.el --- Init File -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2023 Burgess Chang

;; URL: https://github.com/brsvh/emacs.d
;; Keywords: internal
;; Package-Requires: ((emacs "29.1"))
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

;; This file is the first loaded file after Emacs is started.

;; To show the top-level functions and variable declarations in each
;; section, run M-x occur with the following query: ^;;;;* \|^(.

;;; Code:

(use-package use-package
  :init
  ;; When employing the `:hook' keyword to delegate a task to the hook,
  ;; `use-package' will utilize an abbreviated hook name. To illustrate,
  ;; `c-mode-hook' is truncated as `c-mode', and `emacs-startup-hook' is
  ;; truncated as `emacs-startup'. Given my occasional propensity for
  ;; oversight, which may lead to inconsistencies, it is imperative to
  ;; ensure that `use-package' invariably employs the accurate hook
  ;; name.
  (setq use-package-hook-name-suffix nil)

  ;; The `:ensure' keyword of `use-package' conventionally employs
  ;; `package' to verify the installation status of the package. In
  ;; reality, I am currently utilizing twist to manage all ELPA, which
  ;; results in the execution of the superfluous
  ;; `package-refresh-contents' during the startup process,
  ;; circumventing this occurrence.
  (setq use-package-ensure-function
        #'(lambda (name args _state &optional _no-refresh)
            "Do nothing when the need for ensures package installation."
            t)))

(use-package vertico
  :ensure t
  :hook (after-init-hook . vertico-mode))
