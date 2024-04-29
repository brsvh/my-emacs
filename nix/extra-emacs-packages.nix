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
  consult
  dash
  diff-hl
  modus-themes
  setup
  svg-lib
  transient
  yasnippet
])
++ (with manualPackages; [ my.on ])
++ (with melpaPackages; [
  apheleia
  benchmark-init
  company
  f
  hl-todo
  magit
  mwim
  nix-ts-mode
  page-break-lines
  rainbow-delimiters
  smartparens
  svg-tag-mode
])
++ (with nongnuPackages; [
  editorconfig
  hl-block-mode
  yasnippet-snippets
])
