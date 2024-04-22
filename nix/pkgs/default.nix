final: prev:
let
  mkMyEmacsScope = final.callPackage ./my-emacs { };
in
rec {
  my-emacs = mkMyEmacsScope { };

  my-emacs-stable = mkMyEmacsScope { branch = null; };

  my-emacs-unstable = mkMyEmacsScope { branch = "unstable"; };
}
