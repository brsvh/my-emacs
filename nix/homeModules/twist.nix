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
packages:
{ config
, lib
, pkgs
, ...
}:
with builtins;
with lib;
let
  cfg = config.brsvh.emacs;

  pkgs' = packages."${pkgs.system}";

  brsvh-emacs = config.programs.emacs-twist.config;

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

  dependencies = import ../dependencies.nix pkgs;

  prelude = import ../prelude.nix pkgs;
in
{
  options = {
    brsvh = {
      emacs = {
        enable = mkOption {
          type = types.bool;
          default = false;
          description = ''
            Whether the Emacs Configuration is used.
          '';
        };

        platform = mkOption {
          type = types.enum
            [
              "console"
              "wayland"
              "x11"
            ];
          default = "wayland";
          description = ''
            What display server protocol the Emacs configuration will
            run on.
          '';
        };

        directory = mkOption {
          type = types.str;
          description = ''
            Relative path in string to user-emacs-directory from the
            home directory
          '';
          default = ".config/emacs";
          example = ".local/share/emacs";
        };

        extraInitConfig = mkOption {
          type = types.str;
          default = "";
          description = ''
            Extra configuration will be append to the user-init-file of
            GNU Emacs.
          '';
        };

        extraPackages = mkOption {
          type = types.listOf types.str;
          default = [ ];
          description = ''
            Extra Packages will be add to Emacs environment.
          '';
        };

        mail = {
          aliases = mkOption {
            type = types.str;
            default = "";
            description = ''
              The aliases of mail commands.

              The content typically should be in same format as the
              .mailrc file used by the Mail or mailx program.
            '';
          };

          signature = mkOption {
            type = types.str;
            default = "";
            description = ''
              The signature of mail.
            '';
          };
        };

        overrides = mkOption {
          type = types.unspecified;
          default = { };
          description = ''
            Overriding the input of Emacs.
          '';
        };
      };
    };
  };

  config = mkIf cfg.enable {
    home = {
      file = listToAttrs (
        [
          {
            name = "${cfg.directory}/init.el";
            value = {
              source = "${(genInitFile brsvh-emacs)}/init.el";
            };
          }
          {
            name = "${cfg.directory}/early-init.el";
            value = {
              source = pkgs'.early-init-file;
            };
          }
          {
            name = "${cfg.directory}/gnus.el";
            value = {
              source = pkgs'.gnus-init-file;
            };
          }
          {
            name = "${cfg.directory}/mail/mailrc";
            value = {
              text = cfg.mail.aliases;
            };
          }
          {
            name = "${cfg.directory}/mail/signature";
            value = {
              text = cfg.mail.signature;
            };
          }
        ]
      );

      packages = dependencies;
    };

    programs = {
      emacs-twist = {
        enable = true;

        name = "emacs";

        directory = cfg.directory;

        createManifestFile = true;

        config =
          let
            args = {
              inherit prelude;

              extraPackages = cfg.extraPackages;
              inputOverrides = cfg.overrides;
              postlude = cfg.extraInitConfig;
            };
          in
          if cfg.platform == "console"
          then pkgs'.brsvh-emacs-nogui.override (_: args)
          else
            if cfg.platform == "wayland"
            then pkgs'.brsvh-emacs-pgtk.override (_: args)
            else pkgs'.brsvh-emacs.override (_: args);

        serviceIntegration = {
          enable = mkDefault true;
        };

        emacsclient = {
          enable = mkDefault true;
        };
      };

      mu = {
        enable = true;
      };

      password-store = {
        enable = true;
      };
    };

    services = {
      emacs = {
        client = {
          enable =
            config.programs.emacs-twist.serviceIntegration.enable;
        };
      };
    };
  };
}
