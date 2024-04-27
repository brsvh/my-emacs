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
{
  lib,
  lndir,
  makeWrapper,
  runCommandLocal,
  stdenv,
}:
with lib;
{
  extraEmacsPackages,
  extraPackages,
  initDirectory,
  plainEmacs,
  vanillaEmacs,
  ...
}:
stdenv.mkDerivation {
  inherit (vanillaEmacs) meta;

  name = "my-" + vanillaEmacs.name;

  allowSubstitutes = false;
  preferLocalBuild = true;

  buildInputs = [
    initDirectory
    plainEmacs
    lndir
    makeWrapper
  ];

  phases = [ "installPhase" ];

  installPhase = ''
    runHook preInstall

    mkdir $out
    ${lndir}/bin/lndir -silent ${plainEmacs} $out

    mv $out/bin/emacs $out/bin/emacs-unwrapped

    makeWrapper $out/bin/emacs-unwrapped $out/bin/emacs \
      --prefix PATH : ${makeBinPath extraPackages} \
      --add-flags "--init-directory=${initDirectory}"

    runHook postInstall
  '';
}
