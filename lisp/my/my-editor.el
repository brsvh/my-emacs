;;; my-editor.el --- Editing enhancements of My Emacs -*- lexical-binding: t -*-

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

;; This file provides enhanced settings for using Emacs as an editor,
;; with the core objectives being simplicity, speed, power, and
;; modernity.

;;; Code:

(require 'my-core)

(cl-eval-when (compile)
  (require 'mwim))



(defun my-editor-guess-file-mode ()
  "Guess major mode when saving a file in `fundamental-mode'.

Likely, something has changed since the buffer was opened. e.g. A
shebang line or file path may exist now."
  (when (eq major-mode 'fundamental-mode)
    (let ((buffer (or (buffer-base-buffer) (current-buffer))))
      (and (buffer-file-name buffer)
           (eq buffer (window-buffer (selected-window)))
           (set-auto-mode)
           (not (eq major-mode 'fundamental-mode))))))



;;;
;; Files:

(setup files
  (:with-hook after-save-hook
    (:hook #'my-editor-guess-file-mode)))



;;;
;; Goto:

(setup mwim
  (:with-map global-map
    (:keymap-set
     "<remap> <move-beginning-of-line>" #'mwim-beginning-of-code-or-line
     "<remap> <move-end-of-line>" #'mwim-end-of-code-or-line)))



(provide 'my-editor)
;;; my-editor.el ends here
