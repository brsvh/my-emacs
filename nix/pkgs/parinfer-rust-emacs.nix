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
  fetchFromGitHub,
  lib,
  llvmPackages,
  rustPlatform,
}:

rustPlatform.buildRustPackage rec {
  pname = "parinfer-rust-emacs";
  version = "0.4.6";

  src = fetchFromGitHub {
    owner = "justinbarclay";
    repo = "parinfer-rust-emacs";
    rev = "7ef910ab0bf453ce866153e7bf51c80fc1168a3f";
    hash = "sha256-SNs/75beomxvexfE4+3v/l9Xl5w5SY0EWcORHvRitOw=";
  };

  cargoHash = "sha256-LmfcY9iR7BGh3dF/raSZTIwburtaQRI3I3XvOZG343M=";

  nativeBuildInputs = [
    llvmPackages.clang
    rustPlatform.bindgenHook
  ];

  meta = with lib; {
    description = "Infer parentheses for Clojure, Lisp, and Scheme";

    mainProgram = "parinfer-rust";

    homepage = "https://github.com/justinbarclay/parinfer-rust-emacs";

    license = licenses.isc;
    # maintainers = with maintainers; [ brsvh ];
  };
}
