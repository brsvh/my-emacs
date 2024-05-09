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
final: prev:
let
  inherit (prev) callPackage lib;

  mkMyEmacsScope = callPackage ./my-emacs { };
in
rec {
  my-emacs = my-emacs-master;

  my-emacs-master = lib.makeOverridable mkMyEmacsScope { branch = "master"; };

  my-emacs-stable = my-emacs-master.override { branch = null; };

  my-emacs-unstable = my-emacs-master.override { branch = "unstable"; };

  parinfer-rust-emacs = callPackage ./parinfer-rust-emacs.nix { };
}
