;;; my-ui.el --- UI enhancements of My Emacs -*- lexical-binding: t -*-

;; Copyright (C) 2022-2024 Burgess Chang

;; Author: Burgess Chang <bsc@brsvh.org>
;; Keywords: local
;; Package-Requires: ((emacs "29.1"))
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

;; This file provides enhancements to the user interface for my-emacs,
;; including improvements to the appearance, interaction, and default
;; behaviors of Emacs.

;;; Code:

(require 'my-core)

(cl-eval-when (compile)
  (require 'hl-todo)
  (require 'modus-themes)
  (require 'page-break-lines)
  (require 'svg-tag-mode))

(defun my-theme-is-modus (&rest _)
  "Return non-nil if current theme is belong to Modus Themes, else nil."
  (member my-theme '(modus-operandi
                     modus-operandi-tinted
                     modus-vivendi
                     modus-vivendi-tinted)))

(defun my-modus-themes-enable-p (&rest _)
  "Return non-nil if current theme is belong to Modus Themes, else nil."
  (cl-some #'(lambda (theme)
               (member theme '(modus-operandi
                               modus-operandi-tinted
                               modus-vivendi
                               modus-vivendi-tinted)))
           custom-enabled-themes))



;;;
;; Appearance:

(setup my-themes
  (:set
   my-theme 'modus-operandi-tinted
   my-light-theme 'modus-operandi-tinted
   my-dark-theme 'modus-vivendi-tinted)
  (:first-ui my-theme-setup))

(setup modus-themes
  (:only-if (my-theme-is-modus))
  (:set
   ;; Reload modus themes when its option's values is changed.
   modus-themes-custom-auto-reload t)

  (:when-loaded
    (:set
     ;; Use bold and italic for code syntax highlighting.
     modus-themes-bold-constructs t
     modus-themes-italic-constructs t

     ;; Use `fixed-pitch' face for Org tables and code blocks.
     modus-themes-mixed-fonts t

     ;; Use bold prompts.
     modus-themes-prompts '(bold)

     ;; Change boldness of completion faces.
     modus-themes-completions '((matches . (extrabold))
                                (selection . (semibold
                                              fitalic
                                              text-also)))

     ;; Set different font sizes for headings of various levels.
     modus-themes-headings '((0 . (1.40 ultrabold))
                             (1 . (1.30 extrabold))
                             (2 . (1.20 heavy))
                             (3 . (1.10 bold))
                             (t . (1.05 semibold))))

    ;; Override the default faces of modus themes.
    (:snoc modus-themes-common-palette-overrides
           ;; Make `tab-bar' more subtle.
           '(bg-tab-bar bg-active)
           '(bg-tab-current bg-main)
           '(bg-tab-other bg-inactive)
           ;; Make Mode Line borderless.
           '(border-mode-line-active unspecified)
           '(border-mode-line-inactive unspecified))))

(setup hightlight-todo
  ;; Highlight keywords such as FIXME, TODO, REVIEW.
  (:first-ui global-hl-todo-mode))

;; Display ^L page breaks as tidy horizontal lines.
(setup page-break-lines
  (:first-ui
   global-page-break-lines-mode))

(provide 'my-ui)
;;; my-ui.el ends here
