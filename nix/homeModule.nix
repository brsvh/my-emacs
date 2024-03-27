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
{ config
, lib
, pkgs
, ...
}:
with builtins;
with lib;
let
  dependencies = import ./dependencies.nix pkgs;

  localFile = pkgs.writeText "my-emacs-local-file" ''
    ;;; local.el --- Local File -*- lexical-binding: t; -*-

    ;; Copyright (C) 2022-2024 Burgess Chang

    ;; Author: Burgess Chang <bsc@brsvh.org>
    ;; Keywords: local
    ;; Package-Requires: ((emacs "29.1"))
    ;; URL: https://github.com/brsvh/my-emacs
    ;; Version: 0.1.0

    ;; This file is part of my-emacs.

    ;; my-emacs is free software: you can redistribute it and/or modify it
    ;; under the terms of the GNU General Public License as published by the
    ;; Free Software Foundation, either version 3 of the License, or (at
    ;; your option) any later version.

    ;; my-emacs is distributed in the hope that it will be useful, but
    ;; WITHOUT ANY WARRANTY; without even the implied warranty of
    ;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    ;; General Public License for more details.

    ;; You should have received a copy of the GNU General Public License
    ;; along with my-emacs.  If not, see <https://www.gnu.org/licenses/>.

    ;;; Commentary:

    ;; This file is loaded file after My Emacs is initialized.

    ;;; Code:

    (use-package emacs
      :no-require t
      :init
      (-snocq treesit-extra-load-path "${
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
              (
                pipe
                  pkgs.tree-sitter-grammars
                  [
                    (filterAttrs (name: _: name != "recurseForDerivations"))
                    attrValues
                  ]
              )
          )
      }"))

    (use-package parinfer-rust-mode
      :no-require t
      :init
      (setq parinfer-rust-library-directory "${pkgs.parinfer-rust}/lib/"
            parinfer-rust-library "${pkgs.parinfer-rust}/lib/libparinfer_rust.so"
            parinfer-rust-auto-download nil)
    	      (require 'parinfer-rust parinfer-rust-library 'noerror))

    ${config.programs.my-emacs.localConfig}

    ;;; local.el ends here
  '';
in
{
  options.programs.my-emacs = {
    enable = mkOption {
      type = types.bool;
      default = false;
      description = ''
        Whether the Emacs Configuration is used.
      '';
    };

    directory = mkOption {
      type = types.str;
      description = ''
        Relative path in string to user-emacs-directory from the
        home directory
      '';
      default = ".config/emacs";
      example = ".emacs.d";
    };

    extraPackages = mkOption {
      type = types.listOf types.package;
      default = with pkgs; [ imagemagick ];
      description = ''
        Extra Packages will be add to home environment.
      '';
    };

    localConfig = mkOption {
      type = types.str;
      default = "";
      description = ''
        Extra configuration will be append to the local.el file.
      '';
    };

    serviceIntegration = mkOption {
      type = types.bool;
      default = false;
      description = ''
        Whether the Emacs Server is used.
      '';
    };

    windowSystem = mkOption {
      type = types.enum
        [
          "none"
          "pgtk"
          "x11"
        ];
      default = "none";
      description = ''
        What display server protocol the Emacs configuration will
        run on.
      '';
    };
  };

  config = mkIf config.programs.my-emacs.enable
    {
      home = {
        file = listToAttrs (
          [
            {
              name = "${config.programs.my-emacs.directory}/.dir-locals.el";
              value = {
                source = ../.dir-locals.el;
              };
            }
            {
              name = "${config.programs.my-emacs.directory}/early-init.el";
              value = {
                source = ../lisp/early-init.el;
              };
            }
            {
              name = "${config.programs.my-emacs.directory}/etc";
              value = {
                source = ../etc;
                recursive = true;
              };
            }
            {
              name = "${config.programs.my-emacs.directory}/init.el";
              value = {
                source = ../lisp/init.el;
              };
            }
            {
              name = "${config.programs.my-emacs.directory}/lisp";
              value = {
                source = ../lisp;
                recursive = true;
              };
            }
            {
              name = "${config.programs.my-emacs.directory}/local.el";
              value = {
                source = localFile;
              };
            }
            {
              name = "${config.programs.my-emacs.directory}/site-lisp";
              value = {
                source = ../site-lisp;
                recursive = true;
              };
            }
          ]
        );

        packages = dependencies ++ config.programs.my-emacs.extraPackages;
      };

      programs = {
        emacs = {
          enable = true;
          package =
            if config.programs.my-emacs.windowSystem == "none"
            then pkgs.emacs-git-nox
            else
              if config.programs.my-emacs.windowSystem == "pgtk"
              then pkgs.emacs-pgtk
              else
                if config.programs.my-emacs.windowSystem == "x11"
                then pkgs.emacs-git
                else pkgs.emacs-git;
        };
      };

      services = {
        emacs = {
          client = {
            enable =
              config.programs.my-emacs.serviceIntegration.enable;
          };
        };
      };
    };
}
