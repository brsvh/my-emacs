# GNU Emacs Early Init File



## Piecemeal presets

Certain configurations necessitate presetting prior to the loading of
the `user-init-file`, I establish these settings herein.


### Garbage collection optimization

Emacs Lisp is a programming language equipped with garbage collection
support.  During the initialization phase, the default threshold is not
particularly high, leading to frequent garbage collection.
Consequently, I strive to set a larger garbage collection threshold
during the startup process.

    (setq gc-cons-threshold most-positive-fixnum)
    
    (defun my--restore-gc-cons-threshold ()
      "Restore `gc-cons-threshold' to its default value."
      (setq gc-cons-threshold (default-value 'gc-cons-threshold)))
    
    (add-hook 'after-init-hook #'my--restore-gc-cons-threshold 100)


### Emacs Lisp bytecode loading optimization

Emacs conducts a verification of the existence and accuracy of the
bytecode file when loading Emacs Lisp files.  I employ Twist to manage
the bytecode files of Emacs Lisp, which are almost always accurate,
thereby saving on this portion of the overhead.

    (setq load-prefer-newer noninteractive)


### File storage conventions

I aspire for my file storage to adhere to <init.md> and be configured in accordance with the type of operating
system.

    (defmacro my-operating-system-p (os)
      "Return non-nil if OS corresponds to the current operating system.
    Allowable values for OS (not quoted) are `macOS', `osx',
    `windows', `linux', `unix'."
      (pcase os
        (`unix `(not (memq system-type '(ms-dos windows-nt cygwin))))
        ((or `macOS `osx) `(eq system-type 'darwin))
        (`linux `(not (memq system-type
                            '(darwin ms-dos windows-nt cygwin))))
        (`windows `(memq system-type '(ms-dos windows-nt cygwin)))))
    
    (defmacro my-get-xdg-base-dir (concept)
      "Get the value of corresponds XDG Base Directory CONCEPT.
    Allowable concepts (not quoted) are `cache', `config', `data' and
     `state'."
      (let* ((concepts '((cache . ("XDG_CACHE_HOME" . "~/.cache/"))
                         (config . ("XDG_CONFIG_HOME" . "~/.config/"))
                         (data . ("XDG_DATA_HOME" . "~/.local/share/"))
                         (state . ("XDG_STATE_HOME" . "~/.local/state/")))))
        `(let ((default-cons (cdr (assoc ',concept ',concepts))))
           (expand-file-name
            (or (getenv (car default-cons))
                (cdr default-cons))))))
    
    
    (defconst my-cache-directory (if (my-operating-system-p linux)
                                     (expand-file-name
                                      "emacs/"
                                      (my-get-xdg-base-dir cache))
                                   user-emacs-directory)
      "Directory beneath which additional volatile files are placed.")
    
    (defconst my-config-directory user-emacs-directory
      "Directory beneath which additional config files are placed.")
    
    (defconst my-data-directory (if (my-operating-system-p linux)
                                    (expand-file-name
                                     "emacs/"
                                     (my-get-xdg-base-dir data))
                                  user-emacs-directory)
      "Directory beneath which additional non-volatile files are placed.")
    
    (defconst my-state-directory (if (my-operating-system-p linux)
                                     (expand-file-name
                                      "emacs/"
                                      (my-get-xdg-base-dir state))
                                   user-emacs-directory)
      "Directory beneath which additional state files are placed.")


#### eln-cache storage

Redirect storage location for native compilation, it must be set before
all features are `require` if I want to set the follow custom directory
as the first priority.

    (startup-redirect-eln-cache (concat my-cache-directory "eln-cache/"))


### Default frame layout

Add some essentia layout parameters of frame to preset values, i.e.,
here I turned off **Menu Bar**, **Tool Bar**, and **Scroll Bar**.

    (push (cons 'menu-bar-lines nil) default-frame-alist)
    (push (cons 'tool-bar-lines nil) default-frame-alist)
    (push (cons 'vertical-scroll-bars nil) default-frame-alist)
    (push (cons 'horizontal-scroll-bars nil) default-frame-alist)


## File footer

    (provide 'early-init)
    ;;; early-init.el ends here

