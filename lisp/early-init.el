;;; early-init.el --- Early Init File -*- lexical-binding: t; -*-

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

;; This file is load before normal init file is loaded.

;;; Code:

(defvar early-init--file-name-handler-alist
  file-name-handler-alist
  "The initial `file-name-handler-alist'.")

(let ((default-directory user-emacs-directory))
  (setq gc-cons-threshold most-positive-fixnum)

  (add-hook 'after-init-hook
            #'(lambda (&rest _)
                (let ((default (default-value 'gc-cons-threshold)))
                  (setq gc-cons-threshold default)))
            99)
  
  (setq load-prefer-newer noninteractive)

  (setq file-name-handler-alist nil)

  (add-hook 'after-init-hook
            #'(lambda (&rest _)
                (let ((default early-init--file-name-handler-alist)
                      (prev file-name-handler-alist))
                  (setq file-name-handler-alist
                        (delete-dups (append default prev)))))
            98)
  
  (load (expand-file-name "lisp/my/my-prelude") nil 'nomessage))

(provide 'early-init)
;;; early-init.el ends here
