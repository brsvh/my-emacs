# My GNU Emacs Configuraiton

[![img](https://img.shields.io/badge/cachix-brsvh-blue.svg)](https://brsvh.cachix.org)

my-emacs, my personal portable GNU Emacs configuration, is built using
Nix[^1]. I use Nix to manage all Emacs Lisp packages and runtime
dependencies.

## Overview

my-emacs is built through `nix/pkgs/my-emacs`, and by default, this
scope provides three versions:

- `default`, build with pure GTK support, performing better on Wayland.
- `nogui`, a version without graphical support, typically used in my
  devshell.
- `x11`, the conventional version running on X.org.

Each derivation is generated in multiple stages:

- Stage 0, select the appropriate base Emacs from emacs-overlay[^2],
  depending on the branch and Window System support, referred to as
  `vanillaEmacs`.
- Stage 1, build a clean Emacs containing all Emacs Lisp packages,
  termed `plainEmacs`. The Emacs Lisp package dependencies are comprised
  of a default packages list, an overrideable argument named
  `extraEmacsPackages`, and `nix/extra-emacs-packages.nix`.
- Stage 2, link `plainEmacs` with all runtime dependencies and external
  programs to create a wrapper.

## Getting Started

If you wish to directly glimpse my Emacs configuration, you need to
ensure that you have installed the latest Nix, and then simply execute
the following commands.

If you are a Wayland user, you can use the pgtk version.

``` shell
nix run github:brsvh/my-emacs
```

If you are a character-only (tty) user, you can use the nogui version.

``` shell
nix run github:brsvh/my-emacs#nogui
```

If you need to run on X11, you can use the x11 version.

``` shell
nix run github:brsvh/my-emacs#x11
```

And check the [Cheat Sheet] to see which shortcuts I have used.

[Cheat Sheet]: ./cheat-sheet.md

### Add custom Emacs Lisp package

Create custom Emacs Lisp Packages by adding new derivations in
`nix/pkgs/my-emacs/manual-packages`. All custom packages are by default
within a scope named `manualPackages.my`. Custom packages utilize the
Emacs Trivial Builders from `nixpkgs`.

## License

This work is free.  You can redistribute it and/or modify it under the
terms of The GNU General Public License v3.0.  You should have received
a copy of it.  See the COPYING file for more details.  If you did not
recive it, see <https://www.gnu.org/licenses/> for more details.

[^1]: Nix, <https://nixos.org/>
[^2]: emacs-overlay, <https://github.com/nix-community/emacs-overlay/>

