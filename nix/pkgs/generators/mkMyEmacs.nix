# Copyright (C) 2023-2024 Burgess Chang

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
{
  emacs,
  emacs-git,
  emacs-git-nox,
  emacs-gtk,
  emacs-nox,
  emacs-pgtk,
  emacs-unstable,
  emacs-unstable-nox,
  emacs-unstable-pgtk,
  lib,
  mkMyEmacsWrapper,
  my-emacs-init-directory,
  newScope,
  symlinkJoin,
}:
with builtins;
with lib;
let
  availableEmacsPackages = [
    emacs
    emacs-git
    emacs-git-nox
    emacs-gtk
    emacs-nox
    emacs-pgtk
    emacs-unstable
    emacs-unstable-nox
    emacs-unstable-pgtk
  ];
in
{
  emacsPackage ? emacs-unstable-pgtk,
  extraPackages ? [ ],
  extraEmacsPackages ? (epkgs: [ ]),
  initDirectory ? my-emacs-init-directory,
}:
assert elem emacsPackage availableEmacsPackages;
let
  finalEmacs = symlinkJoin {
    name = "emacs";
    paths = [ (emacsPackage.pkgs.withPackages extraEmacsPackages) ] ++ extraPackages;
  };
in
mkMyEmacsWrapper finalEmacs initDirectory
