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
  init-el = packages.${pkgs.system}.emacsD-init-el;
  early-init-el = packages.${pkgs.system}.emacsD-early-init-el;
in
{
  options = {
    emacs.d = {
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
    };
  };

  config = mkIf config.emacs.d.enable {
    home = {
      file = listToAttrs (
        [
          {
            name = "${config.emacs.d.directory}/init.el";
            value = {
              source = "${init-el}/init.el";
            };
          }
          {
            name = "${config.emacs.d.directory}/early-init.el";
            value = {
              source = "${early-init-el}";
            };
          }
        ]
      );
    };

    programs = {
      emacs-twist = {
        enable = true;

        name = "emacs.d";

        directory = config.emacs.d.directory;

        createManifestFile = true;

        config =
          if config.emacs.d.platform == "wayland"
          then packages.${pkgs.system}.emacsD-pgtk
          else packages.${pkgs.system}.emacsD;

        serviceIntegration = {
          enable = mkDefault true;
        };

        emacsclient = {
          enable = mkDefault true;
        };
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
