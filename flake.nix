# Copyright (C) 2023-2024 Burgess Chang

# This file is part of my-emacs.

# my-emacs is free software: you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by the
# Free Software Foundation, either version 3 of the License, or (at your
# option) any later version.

# my-emacs is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with my-emacs.  If not, see <https://www.gnu.org/licenses/>.
{
  description = "my-emacs - Personal GNU Emacs configuration.";

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
    nix-filter = {
      url = "github:numtide/nix-filter/main";
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
    straight = {
      url = "github:nix-community/nix-straight.el/master";
      flake = false;
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
  };

  outputs =
    { devshell
    , emacs-overlay
    , flake-parts
    , nix-filter
    , nixpkgs
    , pre-commit
    , self
    , treefmt
    , ...
    } @ inputs:
      with builtins;
      with nixpkgs.lib;
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
              my-emacs = {
                imports =
                  [
                    (import ./nix/homeModule.nix { nix-filter = nix-filter.lib; })
                  ];
              };
            };
          };

          perSystem =
            { config
            , final
            , pkgs
            , ...
            }:
            {
              devshells = {
                default = {
                  name = "my-emacs:default";

                  devshell = {
                    startup = {
                      pre-commit-hook = {
                        text = config.pre-commit.installationScript;
                      };
                    };
                  };
                };
              };

              overlayAttrs = emacs-overlay.overlays.default final pkgs;

              packages =
                let
                  inherit (final)
                    emacs-git
                    emacs-git-nox
                    emacs-pgtk
                    symlinkJoin;
                in
                {
                  dependencies = symlinkJoin rec {
                    name = "${pname}-${version}";
                    paths = import ./nix/dependencies.nix pkgs;
                    pname = "my-emacs-dependencies";
                    version = "unstable";
                  };

                  nogui = emacs-git-nox;
                  x11 = emacs-git;
                  pgtk = emacs-pgtk;
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
                };
              };
            };

          systems =
            [
              "x86_64-linux"
            ];
        };
}
