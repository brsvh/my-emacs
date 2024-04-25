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
pkgs: with pkgs; [
  ibm-plex
  lxgw-wenkai
  (nerdfonts.override { fonts = [ "NerdFontsSymbolsOnly" ]; })
  nixfmt-rfc-style
  tsangertype-font-xuansan01-w01
  tsangertype-font-xuansan01-w02
  tsangertype-font-xuansan01-w03
  tsangertype-font-xuansan01-w04
  tsangertype-font-xuansan01-w05
  tsangertype-font-jinkai05-w01
  tsangertype-font-jinkai05-w02
  tsangertype-font-jinkai05-w03
  tsangertype-font-jinkai05-w04
  tsangertype-font-jinkai05-w05
]
