;;; my-play.el --- Play of My Emacs -*- lexical-binding: t -*-

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

;; TODO describe this file.

;;; Code:

(require 'my-prelude)

(use-my core)

(use-my mule)

(use-my security)



(use-my editor)

(use-my project)

(use-my org)

(use-my ui)

(use-my workspace)

(use-my lib
  :config
  (my-load-local-file))



(use-my gtd)

(use-my programming)

(use-my writing)



(use-my dired)

(use-my eshell)

(use-my ibuffer)



(use-my postlude)

(provide 'my-play)
;;; my-play.el ends here
