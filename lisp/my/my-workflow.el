;;; my-workflow.el --- My workflow in Emacs -*- lexical-binding: t -*-

;; Copyright (C) 2022-2024 Burgess Chang

;; Author: Burgess Chang <bsc@brsvh.org>
;; Keywords: extensions
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

;; This file configure my workflows in Emacs, currently focusing solely
;; on Getting Things Done (GTD).

;;; Code:

(require 'my-core)

(cl-eval-when (compile)
  (require 'org)
  (require 'org-agenda)
  (require 'org-capture)
  (require 'window))



;;;
;; Common settings:

(setup org
  (:when-loaded
    (:set
     ;; `org-directory' also set in `my-writing-org', but this file is
     ;; load first, ensure it has correct value anyway.
     org-directory (my-path "~/org")

     ;; Collect everything.
     (append org-agenda-files) (my-path org-directory "inbox.org")

     ;; Collect Events and Meetings.
     (append org-agenda-files) (my-path org-directory "agenda.org")

     ;; Collect tasks of my projects.
     (append org-agenda-files) (my-path org-directory "projects.org")

     ;; My states of task:
     ;; - TODO, tasks planned for today.
     ;; - NEXT, upcoming tasks for the next few days.
     ;; - WAIT, tasks awaiting updates from others.
     ;; - DONE, tasks already completed.
     ;; - CANC, tasks cancelled.
     ;; - FAIL, tasks abandoned.
     org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "WAIT(w)" "|"
                                   "DONE(d)" "CANC(c)" "FAIL(f)")))
    (:snoc
     ;; Add some customized special properties.
     org-special-properties "CREATED")))



;;;
;; Get things done:

(setup org-agenda
  (:autoload org-agenda)
  (:with-map ctl-c-a-map
    (:keymap-set
     ;; Show agenda.
     "a" #'org-agenda)))

(setup org-capture
  (:autoload org-capture)
  (:with-map ctl-c-a-map
    (:keymap-set
     ;; Capture everything.
     "c" #'org-capture))
  (:when-loaded
    (:snoc
     org-capture-templates
     `("e" "Event" entry (file+headline "agenda.org" "Event")
       ,(concat
         "* NEXT %?\n"
         "SCHEDULED: %(org-insert-time-stamp (org-read-date nil t))\n"
         "CREATED: %U"))
     `("i" "Inbox" entry (file+headline "inbox.org" "Inbox")
       ,(concat
         "* TODO %?\n"
         "CREATED: %U"))
     `("m" "Meeting" entry (file+headline "agenda.org" "Meeting")
       ,(concat
         "* NEXT %?\n"
         "SCHEDULED: %(org-insert-time-stamp (org-read-date nil t))\n"
         "CREATED: %U")))))

(setup window
  (:set
   (append display-buffer-alist)
   '("\\*Agenda Commands\\*"
     ;; Prefer to show the select window under the current window.
     (display-buffer-reuse-window display-buffer-below-selected)
     ;; Hide Mode Line.
     (window-parameters (mode-line-format . none))
     ;; Set height to 2/5 of current frame.
     (window-height 0.4))))



(provide 'my-workflow)
;;; my-workflow.el ends here