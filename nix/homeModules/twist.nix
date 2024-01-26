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
  cfg = config.emacs.d;

  emacsD = config.programs.emacs-twist.config;

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

      extraInitConfig = mkOption {
        type = types.str;
        default = "";
        description = ''
          Extra configuration will be append to the user-init-file of
          GNU Emacs.
        '';
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
              source = "${(genInitFile emacsD)}/init.el";
            };
          }
          {
            name = "${cfg.directory}/early-init.el";
            value = {
              source = packages.${pkgs.system}.early-init-file;
            };
          }
        ]
      );

      packages = with pkgs;
        [
          imagemagick
          multimarkdown
        ];
    };

    programs = {
      emacs-twist = {
        enable = true;

        name = "emacs";

        directory = cfg.directory;

        createManifestFile = true;

        config =
          if cfg.platform == "wayland"
          then
            packages.${pkgs.system}.emacsD-pgtk.override
              (
                _:
                { appendToInit = cfg.extraInitConfig; }
              )
          else
            packages.${pkgs.system}.emacsD.override
              (
                _:
                { appendToInit = cfg.extraInitConfig; }
              );

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
