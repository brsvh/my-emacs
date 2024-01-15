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
{ inputs
, ...
}:
{
  imports = [
    inputs.devshell.flakeModule
  ];

  perSystem =
    { pkgs
    , ...
    }:
    {
      devshells = {
        default = {
          name = "emacs.d:default";
          commands =
            [
              {
                name = "git";
                package = pkgs.git;
                category = "development";
              }
              {
                name = "nix";
                package = pkgs.nixUnstable;
                category = "development";
              }
            ];
        };
      };
    };
}