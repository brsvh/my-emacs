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
    , nixpkgs
    , nongnu
    , self
    , twist
    , twist-overrides
    , ...
    } @ inputs:
    let
      earlyInitFile = ./lisp/early-init.el;
      initFile = ./lisp/init.el;
      lockDir = ./elpa;
    in
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
                    import ./nix/homeModules/twist.nix self.packages earlyInitFile
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
              inherit (final) emacsTwist;

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

              makeEmacs =
                { nativeCompileAheadDefault ? true
                , wayland ? true
                , x11 ? !wayland
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
                          else { }
                      );
                in
                (
                  emacsTwist {
                    inherit emacsPackage lockDir nativeCompileAheadDefault;

                    configurationRevision = revision;

                    initFiles =
                      [
                        initFile
                        (
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
                          ''
                        )
                      ];

                    initialLibraries =
                      (
                        import flake-pins
                      ).data.emacs.libraries;

                    registries =
                      [
                        {
                          name = "gnu";
                          type = "elpa";
                          path = gnu.outPath + "/elpa-packages";
                          core-src = emacsPackage.src;
                          auto-sync-only = true;
                        }
                        {
                          name = "nongnu";
                          type = "elpa";
                          path = nongnu.outPath + "/elpa-packages";
                          core-src = emacsPackage.src;
                          auto-sync-only = true;
                        }
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
                    wayland = false;
                    x11 = true;
                  }
                );
            in
            {
              apps = emacsD.makeApps { lockDirName = "elpa"; };

              overlayAttrs =
                (
                  emacs-overlay.overlays.default final pkgs
                ) //
                (
                  twist.overlays.default final pkgs
                ) //
                { inherit emacsD emacsD-wayland emacsD-x11; };

              packages = {
                inherit emacsD;

                pgtk = emacsD-wayland // {
                  wrappers = optionalAttrs pkgs.stdenv.isLinux {
                    tmpdir =
                      pkgs.callPackage ./nix/wrapper.nix { }
                        "emacs.d"
                        emacsD-wayland;
                  };
                };

                x11 = emacsD-x11 // {
                  wrappers = optionalAttrs pkgs.stdenv.isLinux {
                    tmpdir =
                      pkgs.callPackage ./nix/wrapper.nix { }
                        "emacs.d"
                        emacsD-x11;
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
