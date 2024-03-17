;;; shackle+.el --- Extra extensions for `shackle' -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Burgess Chang

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

;; These are extensions that provide additional features of `shackle'.

;;; Code:

(require 'shackle)

(defvar shackle+--popup-window-list nil
  "All popup windows.")

(defvar-local shackle+--current-popup-window nil
  "Current popup window.")

(put 'shackle+--current-popup-window 'permanent-local t)

;;;###autoload
(defun shackle+-last-popup-buffer ()
  "View last popup buffer."
  (interactive)
  (ignore-errors
    (display-buffer shackle-last-buffer)))

;;;###autoload
(defun shackle+-autoclose (fn buffer alist plist)
  (let ((window (funcall fn buffer alist plist)))
    (setq shackle+--current-popup-window window)

    (when (plist-get plist :autoclose)
      (push (cons window buffer) shackle+--popup-window-list))
    window))

;;;###autoload
(defun shackle+-quit (&rest _)
  "Close current popup window via `C-g'."
  (setq shackle+--popup-window-list
    (cl-loop for (window . buffer) in shackle+--popup-window-list
      if (and (window-live-p window)
           (equal (window-buffer window) buffer))
      collect (cons window buffer)))
  ;; `C-g' can deactivate region
  (when (and (called-interactively-p 'interactive)
          (not (region-active-p)))
    (let (window buffer process)
      (if (one-window-p)
        (progn
          (setq window (selected-window))
          (when (equal (buffer-local-value 'shackle+--current-popup-window
                         (window-buffer window))
                  window)
            (winner-undo)))
        (progn
          (setq window (caar shackle+--popup-window-list))
          (setq buffer (cdar shackle+--popup-window-list))
          (when (and (window-live-p window)
                  (equal (window-buffer window) buffer))
            (setq process (get-buffer-process buffer))
            (when (process-live-p process)
              (kill-process process))
            (delete-window window)

            (pop shackle+--popup-window-list)))))))

(advice-add #'keyboard-quit :before #'shackle+-quit)

(advice-add #'shackle-display-buffer :around #'shackle+-autoclose)

(provide 'shackle+)
;;; shackle+.el ends here
