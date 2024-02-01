pkgs:
''
  (setq parinfer-rust-library-directory "${pkgs.parinfer-rust}/lib/"
        parinfer-rust-library "${pkgs.parinfer-rust}/lib/libparinfer_rust.so"
        parinfer-rust-auto-download nil)
''
