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

    find $TMPDIR -type f -name "*.el" -exec ${emacs}/bin/emacs --batch -f batch-byte-compile {} \;
  '';

  installPhase = ''
    runHook preInstall

    mkdir -p $out;

    cp $TMPDIR/{early-,}init.el{,c} $out/;
    cp -r $TMPDIR/{etc,lisp,site-lisp} $out/;

    runHook postInstall
  '';
}
