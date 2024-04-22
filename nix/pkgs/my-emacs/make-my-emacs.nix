{
  lib,
  lndir,
  makeWrapper,
  runCommandLocal,
  stdenv,
}:
with lib;
{
  emacs,
  extraEmacsPackages,
  extraPackages,
  initDirectory,
  ...
}:
stdenv.mkDerivation {
  inherit (emacs) meta;

  name = "my-" + emacs.name;

  buildInputs = [
    initDirectory
    emacs
    lndir
    makeWrapper
  ];

  phases = [ "installPhase" ];

  installPhase = ''
    runHook preInstall

    mkdir $out
    ${lndir}/bin/lndir -silent ${emacs} $out

    mv $out/bin/emacs $out/bin/emacs-unwrapped

    makeWrapper $out/bin/emacs-unwrapped $out/bin/emacs \
      --prefix PATH : ${makeBinPath extraPackages} \
      --add-flags "--init-directory=${initDirectory}"

    runHook postInstall
  '';
}
