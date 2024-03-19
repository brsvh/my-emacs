;;; my-core.el --- Heart of My Emacs -*- lexical-binding: t -*-

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

(use-my lib
  :config
  (defvaralias 'ctl-c-map 'my-ctl-c-map)
  (keymap-set global-map "C-c" ctl-c-map)

  ;; Frame operations
  (defvaralias 'ctl-c-5-map 'my-ctl-c-5-map)
  (keymap-set ctl-c-map "5" ctl-c-5-map)

  ;; Action operations
  (defvaralias 'ctl-c-a-map 'my-ctl-c-a-map)
  (keymap-set ctl-c-map "a" ctl-c-a-map)

  ;; Emacs operations
  (defvaralias 'ctl-c-e-map 'my-ctl-c-e-map)
  (keymap-set ctl-c-map "e" ctl-c-e-map)

  ;; Files operations
  (defvaralias 'ctl-c-f-map 'my-ctl-c-f-map)
  (keymap-set ctl-c-map "f" ctl-c-f-map)

  ;; Major mode operations
  (defvaralias 'ctl-c-m-map 'my-ctl-c-m-map)
  (keymap-set ctl-c-map "m" ctl-c-m-map)

  ;; Nix operations
  (defvaralias 'ctl-c-n-map 'my-ctl-c-n-map)
  (keymap-set ctl-c-map "n" ctl-c-n-map)

  ;; Project/Projectile operations
  (defvaralias 'ctl-c-p-map 'my-ctl-c-p-map)
  (keymap-set ctl-c-map "p" ctl-c-p-map)

  ;; Terminal operations
  (defvaralias 'ctl-c-t-map 'my-ctl-c-t-map)
  (keymap-set ctl-c-map "t" ctl-c-t-map)

  ;; VC operations
  (defvaralias 'ctl-c-v-map 'my-ctl-c-v-map)
  (keymap-set ctl-c-map "v" ctl-c-v-map)

  ;; VC (Git) operations
  (defvaralias 'ctl-c-v-g-map 'my-ctl-c-v-g-map)
  (keymap-set ctl-c-v-map "g" ctl-c-v-g-map))

(use-package files
  :keymap-set
  (:ctl-c-e-map
   ("r" . restart-emacs)))

(use-package frame
  :config
  (define-advice display-graphic-p
      (:around (func &optional display) check-window-system)
    "Check `initial-window-system' when no DISPLAY."
    (if display
        (funcall func display)
      initial-window-system)))

(use-package gcmh
  :ensure gcmh
  :pin gnu
  :hook
  (my-init-hook . gcmh-mode))

(use-package package
  :keymap-set
  (:ctl-c-e-map
   ("u" . package-upgrade-all)))

(use-package server
  :config
  (setq server-auth-dir (my-state-path "server/"))

  :hook
  (my-init-hook . my/server-start))



;; Essential libraies:

(use-package cl-lib
  :demand t)

(use-package consult
  :ensure consult
  :pin gnu
  :demand t)

(use-package dash
  :ensure dash
  :pin gnu
  :demand t)

(use-package dash+
  :demand t)

(use-package el-patch
  :vc (:url "https://github.com/radian-software/el-patch.git")
  :demand t)

(use-package f
  :vc (:url "https://github.com/rejeep/f.el.git")
  :demand t)

(use-package on
  :vc (:url "https://gitlab.com/ajgrf/on.el.git")
  :demand t)

(use-package persist
  :ensure persist
  :pin gnu
  :config
  (setq persist--directory-location (my-state-path "persist")))

(use-package promise
  :vc (:url "https://github.com/chuntaro/emacs-promise.git"))

(use-package s
  :vc (:url "https://github.com/magnars/s.el.git")
  :demand t)

(use-package shrink-path
  :vc (:url "https://gitlab.com/bennya/shrink-path.el.git")
  :demand t)

(use-package subr+
  :demand t)

(use-package transient
  :config
  (setq transient-history-file (my-state-path "transient/history.el")
        transient-levels-file (my-state-path "transient/levels.el")
        transient-values-file (my-state-path "transient/values.el")))

(provide 'my-core)
;;; my-core.el ends here
