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
  lndir,
  makeWrapper,
  stdenv,
}:
emacs: init-directory:
stdenv.mkDerivation {
  inherit (emacs) meta name;
  buildInputs = [
    emacs
    lndir
    makeWrapper
  ];
  dontBuild = true;
  dontConfigure = true;
  dontPatch = true;
  dontUnpack = true;
  installPhase = ''
    runHook preInstall
    mkdir $out
    ${lndir}/bin/lndir -silent ${emacs} $out
    mv $out/bin/emacs $out/bin/origin-emacs-wrapper
    makeWrapper $out/bin/origin-emacs-wrapper $out/bin/emacs \
      --add-flags "--init-directory=${init-directory}"
    runHook postInstall
  '';
}
