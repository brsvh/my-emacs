;;; my-ibuffer.el --- `ibuffer' support of My Emacs -*- lexical-binding: t -*-

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

;; TODO describe this file.

;;; Code:

(use-my lib)

(use-package ibuffer
  :keymap-set
  ("<remap> <list-buffers>" . ibuffer)
  :config
  (with-eval-after-load 'winner
    (-snocq winner-boring-buffers
            "*Ibuffer*")))

(use-package nerd-icons-ibuffer
  :vc (:url "https://github.com/seagle0128/nerd-icons-ibuffer.git")
  :config
  (setq nerd-icons-ibuffer-icon t
        nerd-icons-ibuffer-color-icon t
        nerd-icons-ibuffer-icon-size 1.0
        nerd-icons-ibuffer-human-readable-size t)

  :hook
  (ibuffer-mode-hook . nerd-icons-ibuffer-mode))

(use-package ibuffer-project
  :vc (:url "https://github.com/muffinmad/emacs-ibuffer-project.git")
  :hook
  (ibuffer-hook . (lambda ()
                    (setq ibuffer-filter-groups
                          (ibuffer-project-generate-filter-groups))
                    (unless (eq ibuffer-sorting-mode
                                'project-file-relative)
                      (ibuffer-do-sort-by-project-file-relative)))))

(provide 'my-ibuffer)
;;; my-ibuffer.el ends here
