# Copyright (C) 2023 Burgess Chang
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
packages: earlyInitFile:
{ config
, lib
, pkgs
, ...
}:
with lib;
{
  config = mkIf config.programs.emacs-twist.enable {
    programs = {
      emacs-twist = {
        inherit earlyInitFile;

        name = "emacsD";
        directory = ".config/emacs";
        createInitFile = true;
        createManifestFile = true;
        config = packages.${pkgs.system}.emacsD;

        serviceIntegration = {
          enable = mkDefault true;
        };

        emacsclient = {
          enable = mkDefault true;
        };
      };
    };
  };
}
