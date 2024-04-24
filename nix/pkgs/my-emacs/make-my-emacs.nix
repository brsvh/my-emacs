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
