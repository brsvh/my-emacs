{ config
, flake-parts-lib
, lib
, self
, ...
}:
with lib;
let
  inherit (flake-parts-lib) mkSubmoduleOptions;
in
{
  options = {
    flake = mkSubmoduleOptions {
      homeModules = mkOption {
        type = types.lazyAttrsOf types.unspecified;
        default = { };
        apply =
          mapAttrs
            (k: v:
              {
                _file = "${toString self.outPath}/flake.nix#homeModules.${k}";
                imports = [ v ];
              }
            );
        description = ''
          Home Manager modules.

          You may use this for reusable pieces of configuration, service modules, etc.
        '';
      };
    };
  };
}
