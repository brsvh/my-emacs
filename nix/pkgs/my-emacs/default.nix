{
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
  callPackage,
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

  default =
    let
      vanilla =
        if branch == "master" then
          emacs-pgtk
        else if branch == "unstable" then
          emacs-unstable-pgtk
        else
          emacs-gtk;

      emacs = (emacsPackagesFor vanilla).emacsWithPackages extraEmacsPackages;

      initDirectory = mkMyEmacsInitDirectory emacs;
    in
    mkMyEmacs {
      inherit
        emacs
        extraEmacsPackages
        extraPackages
        initDirectory
        vanilla
        ;
    };

  nogui =
    let
      vanilla =
        if branch == "master" then
          emacs-git-nox
        else if branch == "unstable" then
          emacs-unstable-nox
        else
          emacs-nox;

      emacs = (emacsPackagesFor vanilla).emacsWithPackages extraEmacsPackages;

      initDirectory = mkMyEmacsInitDirectory emacs;
    in
    mkMyEmacs {
      inherit
        emacs
        extraEmacsPackages
        extraPackages
        initDirectory
        vanilla
        ;
    };

  x11 =
    let
      vanilla =
        if branch == "master" then
          emacs-git
        else if branch == "unstable" then
          emacs-unstable
        else
          emacs;

      emacs = (emacsPackagesFor vanilla).emacsWithPackages extraEmacsPackages;

      initDirectory = mkMyEmacsInitDirectory emacs;
    in
    mkMyEmacs {
      inherit
        emacs
        extraEmacsPackages
        extraPackages
        initDirectory
        vanilla
        ;
    };
in
makeScope newScope (self: {
  inherit default nogui x11;
})
