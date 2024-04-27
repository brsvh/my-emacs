;;; my-project.el --- Project management of My Emacs -*- lexical-binding: t -*-

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

;; This file dictates management of my projects, including how to
;; organize the files and content within the project, how to handle
;; version control.

;;; Code:



;;;
;; VCS:

;;;
;; Git VCS:

(setup magit
  (:option magit-define-global-key-bindings nil)
  (:with-map my-ctl-c-v-g-map
    (:keymap-set
      "d" #'magit-dispatch
      "s" #'magit-status)))

(provide 'my-project)
;;; my-project.el ends here
