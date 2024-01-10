;;; early-init.el --- Early Init File -*- lexical-binding: t; -*-

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

;; This file is load before normal init file is loaded.

;;; Code:

;; Add some essentia layout parameters of frame to preset values, and
;; ensure the minor modes corresponding to follow values are disabled.
(push (cons 'menu-bar-lines nil) default-frame-alist)
(push (cons 'tool-bar-lines nil) default-frame-alist)
(push (cons 'vertical-scroll-bars nil) default-frame-alist)
(push (cons 'horizontal-scroll-bars nil) default-frame-alist)
(setq-default menu-bar-mode nil tool-bar-mode nil scroll-bar-mode nil)