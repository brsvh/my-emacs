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

  nixConfig = {
    experimental-features =
      [
        "flakes"
        "nix-command"
        "repl-flake"
      ];

    extra-substituters =
      [
        "https://nix-community.cachix.org"
        "https://brsvh.cachix.org"
      ];

    extra-trusted-public-keys =
      [
        "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
        "brsvh.cachix.org-1:DqtlvqnpP9g39l8Eo74AXRftGx1KJLid/ViADTNgDNE="
      ];
  };

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
      url = "github:NixOS/nixpkgs/nixos-23.11";
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
    { devshell
    , emacs-overlay
    , flake-parts
    , flake-pins
    , gnu
    , melpa
    , nixpkgs
    , nongnu
    , org-babel
    , pre-commit
    , self
    , treefmt
    , twist
    , twist-overrides
    , ...
    } @ inputs:
    flake-parts.lib.mkFlake
      { inherit inputs; }
      {
        imports =
          [
            devshell.flakeModule
            flake-parts.flakeModules.easyOverlay
            pre-commit.flakeModule
            treefmt.flakeModule
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
          { config
          , final
          , pkgs
          , self'
          , ...
          }:
            with builtins;
            with nixpkgs.lib;
            let
              inherit (final) emacs-git emacs-git-nox emacs-pgtk;
              inherit (final) tree-sitter-grammars;
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

              postInitFile = el:
                pkgs.writeText "init-postlude.el" el;

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

              defaultInputOverrides = {
                mu4e = _: _: {
                  src = pkgs.mu.mu4e;
                };
              };

              mkEmacs =
                { appendToInit ? ""
                , extraPackages ? [ ]
                , inputOverrides ? defaultInputOverrides
                , nativeCompileAheadDefault ? true
                , pgtk ? false
                , x ? true
                , nogui ? !(pgtk || x)
                , ...
                }:
                let
                  emacsPackage =
                    if nogui
                    then emacs-git-nox
                    else if pgtk
                    then emacs-pgtk
                    else emacs-git;
                in
                (
                  emacsTwist {
                    inherit
                      emacsPackage
                      extraPackages
                      inputOverrides
                      nativeCompileAheadDefault;

                    configurationRevision = revision;

                    initFiles =
                      [
                        initFile
                        (postInitFile appendToInit)
                        initTreeSitterFile
                      ];

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

              brsvh-emacs = makeOverridable mkEmacs { };

              brsvh-emacs-pgtk = brsvh-emacs.override
                (
                  _:
                  {
                    pgtk = true;
                  }
                );

              brsvh-emacs-nogui = brsvh-emacs.override
                (
                  _:
                  {
                    nogui = true;
                  }
                );

              genInitFile = emacs:
                pkgs.runCommandLocal "user-init-file" { } ''
                  mkdir -p $out
                  touch $out/init.el
                  for file in ${concatStringsSep " " emacs.initFiles}
                  do
                  cat "$file" >> $out/init.el
                  echo >> $out/init.el
                  done
                '';

              early-init-file =
                tangleOrgBabelFile "early-init-file" ./org/early-init.org {
                  languages = [ "emacs-lisp" ];
                };

              gnus-init-file =
                tangleOrgBabelFile "gnus-init-file" ./org/gnus.org {
                  languages = [ "emacs-lisp" ];
                };

              createApp = env:
                assert pkgs.stdenv.isLinux;
                pkgs.callPackage ./nix/wrappers/app.nix
                  {
                    early-init = early-init-file;
                    gnus-init = gnus-init-file;
                    init = (genInitFile env);
                  }
                  "emacs"
                  env;

              createBatch = env:
                assert pkgs.stdenv.isLinux;
                pkgs.callPackage ./nix/wrappers/batch.nix
                  {
                    early-init = early-init-file;
                    gnus-init = gnus-init-file;
                    init = (genInitFile env);
                  }
                  "emacs"
                  env;
            in
            {
              apps = brsvh-emacs.makeApps { lockDirName = "elpa"; };

              devshells = {
                default = {
                  name = "emacs.d:default";

                  devshell = {
                    startup = {
                      pre-commit-hook = {
                        text = config.pre-commit.installationScript;
                      };
                    };
                  };

                  commands =
                    [
                      {
                        name = "git";
                        package = pkgs.git;
                        category = "development";
                      }
                      {
                        name = "nix";
                        package = pkgs.nixUnstable;
                        category = "development";
                      }
                      {
                        name = "emacs";
                        package = self'.packages.nogui;
                        help = "The extensible, customizable GNU text editor";
                        category = "development";
                      }
                    ];

                  env =
                    [
                      {
                        name = "EDITOR";
                        value = "emacs";
                      }
                    ];
                };
              };

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
                { inherit brsvh-emacs brsvh-emacs-pgtk brsvh-emacs-nogui; };

              packages = {
                inherit
                  brsvh-emacs
                  brsvh-emacs-nogui
                  brsvh-emacs-pgtk
                  early-init-file
                  gnus-init-file;

                default = self'.packages.x11;

                batch = createBatch brsvh-emacs-nogui;

                nogui = createApp brsvh-emacs-nogui;

                pgtk = createApp brsvh-emacs-pgtk;

                x11 = createApp brsvh-emacs;
              };

              pre-commit = {
                check = {
                  enable = true;
                };
                settings = {
                  hooks = {
                    nixpkgs-fmt = {
                      enable = true;
                    };
                  };
                };
              };

              treefmt = {
                flakeFormatter = true;
                projectRootFile = "flake.nix";

                programs = {
                  nixpkgs-fmt = {
                    enable = true;
                  };

                  shellcheck = {
                    enable = true;
                  };

                  shfmt = {
                    enable = true;
                  };

                  yamlfmt = {
                    enable = true;
                  };
                };

                settings = {
                  global = {
                    excludes =
                      [
                        "elpa/**"
                      ];
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
