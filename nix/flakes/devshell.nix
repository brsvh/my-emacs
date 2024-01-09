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
