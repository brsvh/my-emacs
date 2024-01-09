{ ... }:
{
  imports =
    [
      ./devshell.nix
      ./flake-parts.nix
      ./pre-commit.nix
      ./treefmt.nix
    ];
}
