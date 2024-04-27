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
{ lib, stdenv }:
emacs:
with builtins;
with lib;
stdenv.mkDerivation {
  name = "my-emacs-init-directory";

  allowSubstitutes = false;
  preferLocalBuild = true;

  buildInputs = [ emacs ];

  src = ../../../.;

  phases = [
    "unpackPhase"
    "buildPhase"
    "installPhase"
  ];

  unpackPhase = ''
    :
  '';

  buildPhase = ''
    mkdir -p $TMPDIR;

    cp -r $src/{etc,lisp,site-lisp} $TMPDIR/;

    chmod -R u+w $TMPDIR;

    mv $TMPDIR/lisp/{early-,}init.el $TMPDIR/;

    mkdir -p $TMPDIR/.local;

    find $TMPDIR -type f -name "*.el" -exec ${emacs}/bin/emacs -q --no-site-file --eval "(progn (setq gc-cons-threshold most-positive-fixnum load-prefer-newer t) (dolist (dir '(\"$TMPDIR/lisp\" \"$TMPDIR/site-lisp\")) (push dir load-path) (let ((default-directory dir)) (normal-top-level-add-subdirs-to-load-path))) (setq my-cache-directory \"$TMPDIR/.local\" my-config-directory \"$TMPDIR/.local\" my-data-directory \"$TMPDIR/.local\" my-state-directory \"$TMPDIR/.local\" my-prelude--inhibit-update-load-path t)) " --batch -f batch-byte-compile {} \;
  '';

  installPhase = ''
    runHook preInstall

    mkdir -p $out;

    cp $TMPDIR/{early-,}init.el{,c} $out/;
    cp -r $TMPDIR/{etc,lisp,site-lisp} $out/;

    runHook postInstall
  '';
}
