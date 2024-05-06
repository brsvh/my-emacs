# Copyright (C) 2022-2024 Burgess Chang

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
    experimental-features = [
      "flakes"
      "nix-command"
      "repl-flake"
    ];

    extra-substituters = [
      "https://nix-community.cachix.org"
      "https://brsvh.cachix.org"
    ];

    extra-trusted-public-keys = [
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
    git-hooks = {
      url = "github:cachix/git-hooks.nix/master";
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
    inputs@{
      devshell,
      emacs-overlay,
      flake-parts,
      git-hooks,
      nixpkgs,
      treefmt,
      ...
    }:
    with builtins;
    with nixpkgs.lib;
    flake-parts.lib.mkFlake { inherit inputs; } {
      imports = [
        flake-parts.flakeModules.easyOverlay

        devshell.flakeModule
        git-hooks.flakeModule
        treefmt.flakeModule
      ];

      systems = [ "x86_64-linux" ];

      perSystem =
        {
          config,
          final,
          inputs',
          pkgs,
          ...
        }:
        {
          devshells = {
            default = {
              commands = [
                {
                  category = "development";
                  package = pkgs.git;
                }
                {
                  category = "development";
                  package = pkgs.nixUnstable;
                }
                {
                  category = "editor";
                  name = "emacs";
                  package = final.my-emacs.nogui;
                }
              ];

              devshell = {
                packages = [ final.my-emacs.instruments ];

                startup = {
                  pre-commit = {
                    text = config.pre-commit.installationScript;
                  };
                };
              };

              env = [
                {
                  name = "EDITOR";
                  value = "emacs";
                }
              ];
            };
          };

          overlayAttrs =
            let
              emacs = emacs-overlay.overlays.default final pkgs;
              my-emacs = import ./nix/pkgs final pkgs;
            in
            emacs // my-emacs;

          packages =
            let
              inherit (final) my-emacs;
            in
            {
              default = my-emacs.default;
              nogui = my-emacs.nogui;
              tools = my-emacs.instruments;
              x11 = my-emacs.x11;
            };

          pre-commit = {
            check = {
              enable = true;
            };

            settings = {
              hooks = {
                nixfmt = {
                  enable = true;
                  package = pkgs.nixfmt-rfc-style;
                };
              };
            };
          };

          treefmt = {
            flakeFormatter = true;
            projectRootFile = "flake.nix";
            programs = {
              nixfmt = {
                enable = true;
                package = pkgs.nixfmt-rfc-style;
              };
            };
          };
        };
    };
}
