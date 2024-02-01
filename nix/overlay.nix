final: prev:
{
  # parinfer-rust = prev.parinfer-rust.overrideAttrs
  #   (
  #     _: prevAttrs:
  #     rec {
  #       cargoBuildFeatures =
  #         prevAttrs.cargoBuildFeatures ++
  #         [
  #           "emacs"
  #         ];

  #       cargoCheckFeatures = cargoBuildFeatures; 
  #     }
  #   );
}
