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
  config,
  lib,
  pkgs,
  ...
}:
with lib;
let
  cfg = config.programs.my-emacs;
in
{
  options.programs.my-emacs = {
    enable = mkOption {
      type = types.bool;
      default = false;
      description = mdDoc ''
        Whether use my-emacs.
      '';
    };

    extraBinaries = mkOption {
      type = with types; listOf package;
      default = with pkgs; [ ];
      description = ''
        Extra executable binaries will be add to my-emacs.
      '';
    };

    extraConfig = mkOption {
      type = types.str;
      default = "";
      description = mdDoc ''
        Extra configuration will be add to my-emacs.
      '';
    };

    extraEmacsPackages = mkOption {
      type = with types; listOf package;
      default = with pkgs; [ ];
      description = mdDoc ''
        Extra Emacs Lisp packages will be add to my-emacs.
      '';
    };

    extraFonts = mkOption {
      type = with types; listOf package;
      default = with pkgs; [ ];
      description = mdDoc ''
        Extra font packages will be add to my-emacs.
      '';
    };

    extraLibraries = mkOption {
      type = with types; listOf package;
      default = with pkgs; [ ];
      description = mdDoc ''
        Extra library packages will be add to my-emacs.
      '';
    };

    package = mkOption {
      type = types.package;
      default = pkgs.my-emacs.default;
      description = mdDoc ''
        my-emacs package will be used.
      '';
    };
  };

  config = mkIf cfg.enable {
    home = {
      packages = [ cfg.package ];
    };
  };
}
