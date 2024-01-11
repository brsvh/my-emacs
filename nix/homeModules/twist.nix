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

        name = "emacs.d";
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
