# Copyright (C) 2022-2024 Burgess Chang

# This file is part of my-emacs.

# my-emacs is free software: you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by the
# Free Software Foundation, either version 3 of the License, or (at your
# option) any later version.

# my-emacs is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with my-emacs.  If not, see <https://www.gnu.org/licenses/>.
epkgs:
with epkgs;
(with elpaPackages; [
  activities
  consult
  dash
  diff-hl
  dired-git-info
  embark
  embark-consult
  gcmh
  marginalia
  orderless
  popper
  rainbow-mode
  setup
  svg-lib
  transient
  vertico
  yasnippet
])
++ (with manualPackages; [ my.on ])
++ (with melpaPackages; [
  apheleia
  benchmark-init
  company
  diredfl
  doom-modeline
  f
  frameshot
  hl-todo
  ibuffer-project
  magit
  modus-themes
  mwim
  nerd-icons
  nerd-icons-dired
  nerd-icons-ibuffer
  nix-ts-mode
  page-break-lines
  rainbow-delimiters
  rg
  smartparens
  svg-tag-mode
  switch-window
  tabspaces
])
++ (with nongnuPackages; [
  anzu
  edit-indirect
  editorconfig
  hl-block-mode
  markdown-mode
  yasnippet-snippets
])
