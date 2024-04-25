;;; my-mule.el --- Multilingual environment of My Emacs -*- lexical-binding: t -*-

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

;; Enable my Emacs to support multiple language environments, currently
;; supporting:
;;  - English

;;; Code:

(require 'my-core)

(setup mule-cmds
  (:init
    (set-default-coding-systems 'utf-8)
    (set-language-environment "utf-8")
    (prefer-coding-system 'utf-8)))

(setup emacs
  (:option
    word-wrap-by-category t))

(provide 'my-mule)
;;; my-mule.el ends here
