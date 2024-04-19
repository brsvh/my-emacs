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
{ lndir, runCommandLocal }:
runCommandLocal "my-emacs-init-directory"
  {
    buildInputs = [ lndir ];
    src = ../../.;
  }
  ''
    mkdir -p  $out/{etc,lisp,site-lisp};
    ${lndir}/bin/lndir -silent $src/etc $out/etc
    ${lndir}/bin/lndir -silent $src/lisp $out/lisp
    ${lndir}/bin/lndir -silent $src/site-lisp $out/site-lisp

    ln -s $src/lisp/early-init.el $out/early-init.el
    ln -s $src/lisp/init.el $out/init.el
  ''
