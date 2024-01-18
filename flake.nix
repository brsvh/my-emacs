# Copyright (C) 2023-2024 Burgess Chang
#
# This file is part of emacs.d.
#
# emacs.d is free software: you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by the
# Free Software Foundation, either version 3 of the License, or (at your
# option) any later version.
#
# emacs.d is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
# FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
# for more details.
#
# You should have received a copy of the GNU General Public License
# along with emacs.d.  If not, see <https://www.gnu.org/licenses/>.
{
  description = "emacs.d - Personal GNU Emacs configuration.";

  inputs = {
    devshell = {
      url = "github:numtide/devshell/main";
      inputs = {
        flake-utils = {
          follows = "flake-utils";
        };
        nixpkgs = {
          follows = "nixpkgs";
        };
      };
    };
    elisp-helpers = {
      url = "github:emacs-twist/elisp-helpers/master";
      flake = false;
    };
    emacs-overlay = {
      url = "github:nix-community/emacs-overlay/master";
      inputs = {
        flake-utils = {
          follows = "flake-utils";
        };
        nixpkgs = {
          follows = "nixpkgs-unstable";
        };
        nixpkgs-stable = {
          follows = "nixpkgs-stable";
        };
      };
    };
    flake-compat = {
      url = "github:edolstra/flake-compat/master";
      flake = false;
    };
    flake-parts = {
      url = "github:hercules-ci/flake-parts/main";
      inputs = {
        nixpkgs-lib = {
          follows = "nixpkgs";
        };
      };
    };
    flake-pins = {
      url = "github:akirak/flake-pins/master";
      flake = false;
    };
    flake-utils = {
      url = "github:numtide/flake-utils/main";
      inputs = {
        systems = {
          follows = "systems";
        };
      };
    };
    gitignore = {
      url = "github:hercules-ci/gitignore.nix/master";
      inputs = {
        nixpkgs = {
          follows = "nixpkgs";
        };
      };
    };
    gnu = {
      url = "git+https://git.savannah.gnu.org/git/emacs/elpa.git?ref=main";
      flake = false;
    };
    melpa = {
      url = "github:melpa/melpa/master";
      flake = false;
    };
    nixpkgs = {
      follows = "nixpkgs-unstable";
    };
    nixpkgs-stable = {
      url = "github:NixOS/nixpkgs/nixos-23.05";
    };
    nixpkgs-unstable = {
      url = "github:NixOS/nixpkgs/nixos-unstable";
    };
    nongnu = {
      url = "git+https://git.savannah.gnu.org/git/emacs/nongnu.git?ref=main";
      flake = false;
    };
    org-babel = {
      url = "github:emacs-twist/org-babel/master";
    };
    pre-commit = {
      url = "github:cachix/pre-commit-hooks.nix/master";
      inputs = {
        flake-compat = {
          follows = "flake-compat";
        };
        flake-utils = {
          follows = "flake-utils";
        };
        gitignore = {
          follows = "gitignore";
        };
        nixpkgs = {
          follows = "nixpkgs";
        };
        nixpkgs-stable = {
          follows = "nixpkgs-stable";
        };
      };
    };
    systems = {
      url = "github:nix-systems/default/main";
    };
    treefmt = {
      url = "github:numtide/treefmt-nix/main";
      inputs = {
        nixpkgs = {
          follows = "nixpkgs";
        };
      };
    };
    twist = {
      url = "github:emacs-twist/twist.nix/master";
      inputs = {
        elisp-helpers = {
          follows = "elisp-helpers";
        };
      };
    };
    twist-overrides = {
      url = "github:emacs-twist/overrides/master";
    };
  };

  outputs =
    { emacs-overlay
    , flake-parts
    , flake-pins
    , gnu
    , melpa
    , nixpkgs
    , nongnu
    , org-babel
    , self
    , twist
    , twist-overrides
    , ...
    } @ inputs:
    flake-parts.lib.mkFlake
      { inherit inputs; }
      {
        imports =
          [
            ./nix/flakes
          ];

        flake = {
          homeModules = {
            twist = {
              imports =
                [
                  twist.homeModules.emacs-twist
                  (
                    import ./nix/homeModules/twist.nix self.packages
                  )
                ];
            };
          };
        };

        perSystem =
          { final
          , pkgs
          , ...
          }:
            with builtins;
            with nixpkgs.lib;
            let
              inherit (final) emacs-git tree-sitter-grammars;
              inherit (final) emacsTwist tangleOrgBabelFile;

              revision = "${substring 0 8 self.lastModifiedDate}.${
                if self ? rev
                then substring 0 7 self.rev
                else "dirty"
              }";

              grammars =
                pipe
                  tree-sitter-grammars
                  [
                    (filterAttrs (name: _: name != "recurseForDerivations"))
                    attrValues
                  ];

              initFile =
                tangleOrgBabelFile "init.el" ./org/init.org {
                  languages = [ "emacs-lisp" ];
                };

              initTreeSitterFile =
                pkgs.writeText "init-tree-sitter.el" ''
                  (add-to-list 'treesit-extra-load-path "${
                    pkgs.linkFarm "treesit-grammars"
                      (
                        map
                          (
                            drv: {
                              name = "lib${
                                removeSuffix "-grammar" (getName drv)
                              }${
                                pkgs.stdenv.targetPlatform.extensions.sharedLibrary
                              }";
                              path = "${drv}/parser";
                            }
                          )
                          grammars
                      )
                  }/")
                '';

              initFiles =
                [
                  initFile
                  initTreeSitterFile
                ];

              my-milkypostman-elpa = {
                name = "my-melpa";
                type = "melpa";
                path = ./melpa;
              };

              milkypostman-elpa = {
                name = "melpa";
                type = "melpa";
                path = melpa.outPath + "/recipes";
              };

              gnu-elpa = {
                name = "gnu";
                type = "elpa";
                path = gnu.outPath + "/elpa-packages";
                core-src = emacsPackage.src;
                auto-sync-only = true;
              };

              nongnu-elpa = {
                name = "nongnu";
                type = "elpa";
                path = nongnu.outPath + "/elpa-packages";
                core-src = emacsPackage.src;
                auto-sync-only = true;
              };

              makeEmacs =
                { nativeCompileAheadDefault ? true
                , wayland ? false
                , x11 ? false
                , noGui ? !(wayland || x11)
                , ...
                }:
                let
                  emacsPackage =
                    emacs-git.override
                      (
                        _:
                        if wayland
                        then { withPgtk = true; }
                        else
                          if x11
                          then { withGTK3 = true; }
                          else
                            if noGui
                            then { noGui = true; }
                            else { }
                      );
                in
                (
                  emacsTwist {
                    inherit
                      emacsPackage
                      initFiles
                      nativeCompileAheadDefault;

                    configurationRevision = revision;

                    initialLibraries =
                      (
                        import flake-pins
                      ).data.emacs.libraries;

                    lockDir = ./elpa;

                    registries =
                      [
                        my-milkypostman-elpa
                        milkypostman-elpa
                        gnu-elpa
                        nongnu-elpa
                      ];

                    exportManifest = true;
                  }
                ).overrideScope' twist-overrides.overlays.twistScope;

              emacsD = makeOverridable makeEmacs { };

              emacsD-wayland = emacsD.override
                (
                  _:
                  {
                    wayland = true;
                  }
                );

              emacsD-x11 = emacsD.override
                (
                  _:
                  {
                    x11 = true;
                  }
                );

              emacsD-noGui = emacsD.override
                (
                  _:
                  {
                    noGui = true;
                  }
                );

              emacsD-init-el =
                pkgs.runCommandLocal "emacsD-init-el" { } ''
                  mkdir -p $out
                  touch $out/init.el
                  for file in ${concatStringsSep " " emacsD.initFiles}
                  do
                  cat "$file" >> $out/init.el
                  echo >> $out/init.el
                  done
                '';

              emacsD-early-init-el =
                tangleOrgBabelFile "early-init.el" ./org/early-init.org {
                  languages = [ "emacs-lisp" ];
                };
            in
            {
              apps = emacsD.makeApps { lockDirName = "elpa"; };

              overlayAttrs =
                (
                  emacs-overlay.overlays.default final pkgs
                ) //
                (
                  org-babel.overlays.default final pkgs
                ) //
                (
                  twist.overlays.default final pkgs
                ) //
                { inherit emacsD emacsD-wayland emacsD-x11; };

              packages = {
                inherit
                  emacsD
                  emacsD-early-init-el
                  emacsD-init-el
                  emacsD-wayland
                  emacsD-x11
                  emacsD-noGui;

                pgtk = emacsD-wayland // {
                  wrappers = optionalAttrs pkgs.stdenv.isLinux {
                    tmpdir =
                      pkgs.callPackage
                        ./nix/wrapper.nix
                        {
                          early-init = emacsD-early-init-el;
                          init = emacsD-init-el;
                        }
                        "emacs.d"
                        emacsD-wayland;
                  };
                };

                x11 = emacsD-x11 // {
                  wrappers = optionalAttrs pkgs.stdenv.isLinux {
                    tmpdir =
                      pkgs.callPackage
                        ./nix/wrapper.nix
                        {
                          early-init = emacsD-early-init-el;
                          init = emacsD-init-el;
                        }
                        "emacs.d"
                        emacsD-x11;
                  };
                };

                nogui = emacsD-noGui // {
                  wrappers = optionalAttrs pkgs.stdenv.isLinux {
                    tmpdir =
                      pkgs.callPackage
                        ./nix/wrapper.nix
                        {
                          early-init = emacsD-early-init-el;
                          init = emacsD-init-el;
                        }
                        "emacs.d"
                        emacsD-noGui;
                  };
                };
              };
            };

        systems =
          [
            "x86_64-linux"
          ];
      };
}
