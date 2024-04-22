{ runCommandLocal }:
emacs:
runCommandLocal "my-emacs-init-directory"
  {
    buildInputs = [ emacs ];
    src = ../../../.;
  }
  ''
    mkdir -p  $out/{etc,lisp,site-lisp};

    cp -r $src/etc $out/etc;

    cp -r $src/lisp $out/lisp;
    find $out/lisp -type f -name "*.el" -exec ${emacs}/bin/emacs --batch -f batch-byte-compile {} \;
    ln -s $src/lisp/early-init.el $out/early-init.el;
    ln -s $src/lisp/early-init.elc $out/early-init.elc;
    ln -s $src/lisp/init.el $out/init.el;
    ln -s $src/lisp/init.elc $out/init.elc;

    cp -r $src/site-lisp $out/site-lisp;
    find $out/lisp -type f -name "*.el" -exec ${emacs}/bin/emacs --batch -f batch-byte-compile {} \;
  ''
