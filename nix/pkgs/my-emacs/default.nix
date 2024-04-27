{
  callPackage,
  emacs,
  emacs-git,
  emacs-git-nox,
  emacs-gtk,
  emacs-nox,
  emacs-pgtk,
  emacs-unstable,
  emacs-unstable-nox,
  emacs-unstable-pgtk,
  emacsPackagesFor,
  lib,
  newScope,
  pkgs,
}:
with lib;
{
  branch ? "master",
  extraEmacsPackages ? (import ../../extra-emacs-packages.nix),
  extraPackages ? (import ../../extra-packages.nix pkgs),
}:
let
  mkMyEmacs = callPackage ./make-my-emacs.nix { };

  mkMyEmacsInitDirectory = callPackage ./make-my-emacs-init-directory.nix { };

  emacsPackagesFor' =
    drv:
    (emacsPackagesFor drv).overrideScope' (
      finalEpkg: prevEpkg:
      let
        manualPackages = prevEpkg.manualPackages // {
          my = callPackage ./manual-packages {
            inherit (prevEpkg) trivialBuild;
            emacs = drv;
            passedPackages = prevEpkg;
          };
        };
      in
      prevEpkg.overide { inherit manualPackages; }
    );

  getPlainEmacs = drv: (emacsPackagesFor drv).emacsWithPackages extraEmacsPackages;

  default =
    let
      vanillaEmacs =
        if branch == "master" then
          emacs-pgtk
        else if branch == "unstable" then
          emacs-unstable-pgtk
        else
          emacs-gtk;

      plainEmacs = (getPlainEmacs vanillaEmacs);

      initDirectory = mkMyEmacsInitDirectory plainEmacs;
    in
    mkMyEmacs {
      inherit
        extraEmacsPackages
        extraPackages
        initDirectory
        plainEmacs
        vanillaEmacs
        ;
    };

  nogui =
    let
      vanillaEmacs =
        if branch == "master" then
          emacs-git-nox
        else if branch == "unstable" then
          emacs-unstable-nox
        else
          emacs-nox;

      plainEmacs = (getPlainEmacs vanillaEmacs);

      initDirectory = mkMyEmacsInitDirectory plainEmacs;
    in
    mkMyEmacs {
      inherit
        extraEmacsPackages
        extraPackages
        initDirectory
        plainEmacs
        vanillaEmacs
        ;
    };

  x11 =
    let
      vanillaEmacs =
        if branch == "master" then
          emacs-git
        else if branch == "unstable" then
          emacs-unstable
        else
          emacs;

      plainEmacs = (getPlainEmacs vanillaEmacs);

      initDirectory = mkMyEmacsInitDirectory plainEmacs;
    in
    mkMyEmacs {
      inherit
        extraEmacsPackages
        extraPackages
        initDirectory
        plainEmacs
        vanillaEmacs
        ;
    };
in
makeScope newScope (self: {
  inherit default nogui x11;
})
