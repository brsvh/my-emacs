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
            ./nix/flakeModules
            ./nix/flakes
          ];

        flake = {
          homeModules = {
            twist = {
              imports =
                [
                  twist.homeModules.emacs-twist
                  (
                    import ./nix/homeModules self.packages earlyInitFile
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
              inherit (final) emacs-git emacs-pgtk tree-sitter-grammars;
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
                , pgtk ? true
                , ...
                }:
                let
                  emacsPackage =
                    (
                      if pgtk
                      then emacs-pgtk
                      else emacs-git
                    ).override (_:
                      (
                        if pgtk
                        then { }
                        else { withGTK3 = true; }
                      )
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
                          type = "elpa";
                          path = gnu.outPath + "/elpa-packages";
                          core-src = emacsPackage.src;
                          auto-sync-only = true;
                        }
                        {
                          type = "elpa";
                          path = nongnu.outPath + "/elpa-packages";
                          core-src = emacsPackage.src;
                          auto-sync-only = true;
                        }
                      ];

                    exportManifest = true;
                  }
                ).overrideScope' twist-overrides.overlays.twistScope;

              emacs-config = makeOverridable makeEmacs {
                pgtk = true;
              };
            in
            {
              apps = emacs-config.makeApps { lockDirName = "elpa"; };

              overlayAttrs =
                (
                  inputs.emacs-overlay.overlays.default final pkgs
                ) //
                (
                  inputs.twist.overlays.default final pkgs
                ) //
                { inherit emacs-config; };

              packages = {
                inherit emacs-config;

                pgtk = emacs-config // {
                  wrappers = optionalAttrs pkgs.stdenv.isLinux {
                    tmpdir =
                      pkgs.callPackage ./nix/wrapper.nix { }
                        "emacs.d"
                        emacs-config;
                  };
                };

                x11 = (emacs-config.override (_:
                  {
                    pgtk = false;
                  }
                )) // {
                  wrappers = optionalAttrs pkgs.stdenv.isLinux {
                    tmpdir =
                      pkgs.callPackage ./nix/wrapper.nix { }
                        "emacs.d"
                        emacs-config;
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
