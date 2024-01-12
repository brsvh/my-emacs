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
{ writeShellScriptBin
, runCommandLocal
, ...
}: name: emacs-env:
let
  initFile = runCommandLocal "init.el" { } ''
    mkdir -p $out
    touch $out/init.el
    for file in ${builtins.concatStringsSep " " emacs-env.initFiles}
    do
      cat "$file" >> $out/init.el
      echo >> $out/init.el
    done
  '';
in
writeShellScriptBin name ''
  set +u
  set -x

  initdir="$(mktemp --tmpdir -d ${name}-XXX)"

  cleanup() {
    rm -rf "$initdir"
  }

  trap cleanup ERR EXIT

  ln -s ${initFile}/init.el "$initdir/init.el"
  ln -s ${../lisp/early-init.el} "$initdir/early-init.el"

  ${emacs-env}/bin/emacs --init-directory="$initdir" "$@"
''
