
# Table of Contents

-   [About](#orgdd4a5ab)
    -   [Why Emacs?](#org7fd1e5e)
    -   [Why Vanila Emacs?](#orgaa7e27c)
    -   [How literature configuration works?](#org05d125a)
    -   [Why a monolithic configuration?](#org24af1c7)
    -   [Special Thanks](#org42a98c2)
-   [Convention](#org42d4572)
    -   [Name Convention](#orgbe306c5)
    -   [Bind Convention](#org088d051)
    -   [File Convention](#orgdc8a2b0)
    -   [Supported Environment](#org03f164f)
-   [Booting Up](#org04a3bb9)
    -   [Early Init](#org68c43cf)
    -   [Preparation for literature configuration](#org144e531)
    -   [Package Management](#org1f66232)
    -   [Inhibit saving customization to `user-init-file` by appending](#orgdf38b20)
    -   [Syncing to the shell's environment variables](#org0454e28)
    -   [Intelligent garbage collection](#org63b9c4e)
    -   [UTF-8 everywhere](#org3fbaa51)
    -   [Using Emacs as a server](#org4ab6e10)
    -   [Resarting Emacs](#orge73ddb6)
-   [Common Library](#orgf4884dd)
-   [Appearance](#org1af613a)
    -   [Change the default frame layout](#orgc8b9374)
    -   [Change the default startup screen](#orgddc7b95)
    -   [Resize pixelwise](#org0050a87)
    -   [Accessible themes](#orgfb05468)
    -   [Tabs and Ribbons style Mode Line](#orgca3ddff)
    -   [Context Menu](#org6675197)
    -   [Smooth Scrolling](#orgcd698aa)
    -   [Highlighting changes](#org1430af0)
-   [Completion](#org315c874)
    -   [Allow recursive edit in minibuffer](#org824bfe7)
    -   [Hide commands in M-x which do not work in the current mode](#org8876acb)
    -   [Do not allow the cursor in the minibuffer prompt](#org137ca8a)
    -   [Eye-catching indicator when calling `completing-read-multiple`](#orga222e64)
    -   [Match completion with space](#orge3f2474)
    -   [Use vertical completion UI with vertico.el](#org7283764)
    -   [Completion Overlay Region FUnction](#orgee8e181)
    -   [Show mariginalia of completions](#orgde18eed)
-   [Actions in Emacs](#org25d6488)
    -   [Save actions history](#org721c5db)
    -   [Switch buffer with preview](#org6e05076)
    -   [Avaiable actions menu](#org7abb680)
    -   [Help Actions](#org44b7cc2)
    -   [Adjust scrolling distance](#orgc123dc4)
    -   [Mouse actions](#org78c2efe)
    -   [Drag and Drop](#org6dd1e1f)
    -   [Mouse yank at point](#orgecea876)
    -   [Search actions](#org02e8f2e)
    -   [Goto actions](#org47fda81)
-   [Use Emacs as general text editor](#orge68b1b3)
    -   [Automatic Backups](#orge470f69)
    -   [Automatic Saving](#org8f41497)
    -   [Recent Files](#org21a5d81)
    -   [Show basic information of buffer](#orgd5d6030)
    -   [Save the last opened place](#org7d35ff2)
    -   [Replace region with new content](#org110d415)
    -   [Display file name with forward style](#org82128d6)
    -   [Kill ring and Clipboard](#orgb28e512)
-   [File Management](#org5e8dae5)
    -   [Version Control](#orgad7b77e)
-   [Life with `org-mode`](#org491c523)
    -   [Using `org-mode` as a Day Planner](#orga717209)
    -   [Files of Day Planner](#orgd4cda7a)
    -   [Capture and collect tasks](#org21ef27a)
-   [Prose](#orge86410b)
    -   [Generic writing experience improvements for text of Prose](#orge584033)
    -   [Distraction-free writing](#orge9afac8)
    -   [Write article with `org-mode`](#org8e8aafd)
    -   [Write article with LaTeX](#org43a02a3)
-   [Programming](#orgfa55400)
    -   [Generic programming enhancement](#org54edd1a)
    -   [C/C++](#org429744a)
    -   [Emacs Lisp](#org52c8c5d)
    -   [Haskell](#orgd1dc408)
    -   [Rust](#orged938bb)

:header-args: :comment no :lexical t :mkdirp yes :tangle (eval my-profile)


<a id="orgdd4a5ab"></a>

# TODO About


<a id="org7fd1e5e"></a>

## TODO Why Emacs?


<a id="orgaa7e27c"></a>

## TODO Why Vanila Emacs?


<a id="org05d125a"></a>

## TODO How literature configuration works?


<a id="org24af1c7"></a>

## TODO Why a monolithic configuration?


<a id="org42a98c2"></a>

## TODO Special Thanks


<a id="org42d4572"></a>

# Convention

I keep the configuration file style consistent as it grows by conventions for functions, variables, key bindings, and file storage.


<a id="orgbe306c5"></a>

## Name Convention

I use a very simple function and variable naming convention.

-   Public fucntions and variables should be named start with the prefix `my-`.
-   Private or internal fucntions and variables should be named start with the prefix `my--`.


<a id="org088d051"></a>

## Bind Convention

For all commands, I have created and from other packages, I bind them to **C-c** when I want to call them through a series of key combinations.

I defined a keymap named `ctl-c-map`, just like `ctl-x-map`, binding all commands with **C-c** as a prefix key to it.

    (defvar ctl-c-map (make-keymap)
      "Default keymap for C-c commands.")
    
    (keymap-set global-map "C-c" ctl-c-map)

Usually, the major mode command is bound to some key under the prefix key **C-c** and needs to hold down **Control**.  For example, **C-c C-e** is bound to `elisp-eval-buffer` in `emacs-lisp-mode`, **C-c C-l** is bound to `org-insert-link` in `org-mode`, etc.

So I bind the commands I need to the letter keys with **C-c** as the prefix key.  These letter keys are also used for categorization purposes.

    (defvar ctl-c-e-map (make-keymap)
      "Default keymap for C-c e commands.")
    
    (defvar ctl-c-f-map (make-keymap)
      "Default keymap for C-c f commands.")
    
    (defvar ctl-c-o-map (make-keymap)
      "Default keymap for C-c v commands.")
    
    (defvar ctl-c-v-map (make-keymap)
      "Default keymap for C-c v commands.")
    
    (keymap-set ctl-c-map "e" ctl-c-e-map)
    (keymap-set ctl-c-map "f" ctl-c-f-map)
    (keymap-set ctl-c-map "o" ctl-c-o-map)
    (keymap-set ctl-c-map "v" ctl-c-v-map)

These letter keys correspond to the following categories and usages.

-   `e`: commands for Emacs management.
-   `f`: commands for file management.
-   `v`: commands for version control.


<a id="orgdc8a2b0"></a>

## File Convention

The default paths used to store configuration files and persistent data are inconsistent across Emacs packages.  This isn’t just a problem with third-party packages but even with built-in packages.

Some packages put these files directly in `user-emacs-directory` or `$HOME` or in a subdirectory of either of the two or elsewhere.  Furthermore, file names that don’t provide insight into what package might have created them are sometimes used.

So I manage them myself, following the XDG Base Directory specification as much as possible.  

I have grouped the Emacs-related files into four categories.

-   cache file: volatile; created by Emacs Packages.
-   config file: non-volatile; created by me; I care about their content.
-   data file: non-volatile; created by Emacs Packages; crossing session effective.
-   state file: non-volatile; created by Emacs Packages; current session effective.

    (defvar my-config-directory user-emacs-directory
      "Directory beneath which additional config files are placed.")
    
    (defvar my-cache-directory (let ((linux '(berkeley-unix
    					  gnu
    					  gnu/kfreebsd
    					  gnu/linux))
    				 (xdg (getenv "XDG_CACHE_HOME")))
    			     (if (and (memq system-type linux) xdg)
    				 (expand-file-name "emacs/" xdg)
    			       user-emacs-directory))
      "Directory beneath which additional volatile files are placed.")
    
    (defvar my-data-directory (let ((linux '(berkeley-unix
    					 gnu
    					 gnu/kfreebsd
    					 gnu/linux))
    				(xdg (getenv "XDG_DATA_HOME")))
    			    (if (and (memq system-type linux) xdg)
    				(expand-file-name "emacs/" xdg)
    			      user-emacs-directory))
      "Directory beneath which additional non-volatile files are placed.")
    
    (defvar my-state-directory (let ((linux '(berkeley-unix
    					 gnu
    					 gnu/kfreebsd
    					 gnu/linux))
    				(xdg (getenv "XDG_STATE_HOME")))
    			    (if (and (memq system-type linux) xdg)
    				(expand-file-name "emacs/" xdg)
    			      user-emacs-directory))
      "Directory beneath which additional state files are placed.")


<a id="org03f164f"></a>

## Supported Environment

My operating system is GNU/Linux, and my distribution is Arch Linux.  My Emacs configuration always lives in the master branch of the Emacs code repository.

Current verbose build information about my Emacs:

    
    In GNU Emacs 29.0.50 (build 1, x86_64-pc-linux-gnu, GTK+ Version
     3.24.34, cairo version 1.17.6) of 2022-10-26 built on brsvh.org
    Repository revision: e54c3959827eeee3ea60ccaa4918d22b9dce9cc5
    Repository branch: master
    System Description: Arch Linux
    
    Configured using:
     'configure --prefix=/usr --sysconfdir=/etc --libexecdir=/usr/lib
     --localstatedir=/var --mandir=/usr/share/man --with-sound=alsa
     --with-gsettings --without-gconf --with-sound=alsa --with-modules
     --without-compress-install --with-dumping=pdumper --with-pdumper=yes
     --with-native-compilation=aot --with-pgtk --with-xwidgets
     --without-xaw3d --without-libotf --without-m17n-flt
     --enable-link-time-optimization
     '--program-transform-name=s/\([ec]tags\)/\1.emacs/'
     'CFLAGS=-march=x86-64 -mtune=generic -O2 -pipe -fno-plt -fexceptions
     -Wp,-D_FORTIFY_SOURCE=2 -Wformat -Werror=format-security
     -fstack-clash-protection -fcf-protection'
     LDFLAGS=-Wl,-O1,--sort-common,--as-needed,-z,relro,-z,now'

The Emacs version below 29.0.50 will have some errors during startup, so this configuration only works on the master branch.  Likewise, I haven't adapted and tested it for Windows and macOS, so it may work.


<a id="org04a3bb9"></a>

# Booting Up

Let's make some necessary settings to facilitate our further personalizing Emacs.


<a id="org68c43cf"></a>

## Early Init

After Emacs 27.0.50, Emacs supports be configured before eval `user-init-file`.  I did not abuse the early initialization.  I configured some options for `comp`, `package`, and default frame in the early initialization.


### Move Emacs Native Compilation cache to not mess up my emacs.d

Beginning from Emacs 28.0.50, Emacs support native compilation.  It greatly improves the loading speed of Emacs Lisp.  It is configuration free, but I need to move default *eln-cache/* directory under `user-emacs-directory` to follow my [File Convention](#orgdc8a2b0).

    (startup-redirect-eln-cache (concat my-cache-directory "eln-cache/"))


<a id="org144e531"></a>

## Preparation for literature configuration

I used org-mode to write the literature configuration(this file).  It supports extracting code blocks to code files and exporting to other file formats.

The actual loading sequence of the literature configuration is as follows:

1.  Eval init file
2.  Try to extract code blocks to a configuration file(Emacs Lisp file)
3.  Try to eval the configuration file

To try to extract the code block to the configuration file, I declared a function that compares the update time of the literature configuration file and the configuration file to determine if it needs to be extracted.

    (defun my-profile-publish (file target-file)
      "Try to export code from the literate FILE to TARGET-FILE.
    
    Optional argument TARGET-FILE can be used to specify a default
    export file for all source blocks.
    
    Return a list whose CAR is the tangled file name, and CDR is t if
    FILE is tangled, otherwise nil."
      (unless (fboundp 'org-babel-tangle-file)
        (autoload 'org-babel-tangle-file "ob-tangle"))
      (eval-when-compile
        (declare-function org-babel-tangle-file "ob-tangle"))
      (if (or (not (file-exists-p file))
    	  (file-newer-than-file-p my-literate-profile my-profile))
          (cons (car (org-babel-tangle-file file target-file)) t)
        (cons target-file nil)))

Then the literature configuration can be loaded by loading the configuration file.

    (defvar my-literate-profile (concat user-emacs-directory "brsvh.org")
      "Burgess Chang's literate configuration profile.")
    
    (defvar my-profile (concat my-data-directory "lisp/brsvh.el")
      "Burgess Chang's configuration profile.")
    
    (load (car (my-profile-publish my-literate-profile my-profile)) nil t)

**Notice**: the above two code blocks are not extracted into the configuration file, they actually exist in the `user-init-file`.


<a id="org1f66232"></a>

## Package Management

I choose to use the built-in *package.el* because it provides by Emacs distribution and is almost configuration free.

It can be used with a simple `require`, and all installed packages will be activated.

    (require 'package)

Beginning at Emacs 27.0.50, package activation occurs before `user-init-file` is loaded but after `early-init-file`.  So I no longer need to do package activation.  Only when the package activations failed I do it.

    (unless package--activated
      (package-activate-all))

As mentioned above, I need to put the customization of *package.el* that affect package initialization to `early-init-file`.  In fact, I did redirect `package-user-dir` in the `early-init-file`.

    ;; Beginning at Emacs 27.0.50, package initialization occurs before
    ;; `user-init-file' is loaded, but after `early-init-file', so some of
    ;; necessary options of `package' I need to set in `early-init-file'.
    (setq package-user-dir (format "%s%s/"
    			       (concat my-cache-directory "elpa/")
    			       emacs-version)
          package-gnupghome-dir (concat package-user-dir "gnupg/")
          ;; Enable native ompilation support of packages.
          package-native-compile t
          ;; Enable precompute activation actions to speed up package
          ;; activatation.
          package-quickstart t
          package-quickstart-file (concat package-user-dir "loaddefs.el"))


### Emacs Lisp Package Archive

By default, Emacs support downloading and installing ELPA from official archives sites, aka GNU ELPA and Non-GNU ELPA.  But these two archive repositories contain only a relatively small part of the Emacs ecosystem, so I add Milkypostman’s ELPA for convenience get more and more packages.

    (add-to-list 'package-archives
    	     (cons "melpa" (format "http%s://melpa.org/packages/"
    				   (if (gnutls-available-p) "s" "")))
    	     'append)


### Easy-to-use package configuration macro

In the real world of Emacs configuration, you will find many `with-eval-after-load`, `add-hook`, `require` and other blocks of Emacs Lisp.  This will seem dizzying and somewhat lengthy to write.

In the ELPA repositories, you can find several packages designed to simplify the configuration of the Emacs Lisp package by providing the macro wrapper.

The packages are well known are:

-   [use-package.el](https://github.com/jwiegley/use-package)
-   [leaf.el](https://github.com/conao3/leaf.el)
-   [setup.el](https://git.sr.ht/~pkal/setup)

I choose to use [use-package.el](https://github.com/jwiegley/use-package), because of its long maintenance history and more users.

All that needs to be done is to ensure it has been installed from ELPA repositories, and require it.

    (unless (package-installed-p 'use-package)
        (unless (memq 'use-package package-archive-contents)
          (package-refresh-contents))
        (package-install 'use-package))

If interested, you can further read follow references to learn about managing the Emacs Lisp package with other packagers.

-   [Comparison to other package managers](https://github.com/radian-software/straight.el#comparison-to-other-package-managers)
-   [Advanced Emacs Package Management with straight.el](https://systemcrafters.cc/advanced-package-management/using-straight-el/)


#### Keyword conventions for `use-package`

I will not use some keywords for the following reasons.

-   `:bind`, `:bind*`, `:bind-keymap` and `:bind-keymap*`
    
    These keywords actually call the functions `bind-key` and `bind-keys` provided by [bind-key.el](https://github.com/jwiegley/use-package/blob/master/bind-key.el).  The [bind-key.el](https://github.com/jwiegley/use-package/blob/master/bind-key.el) is, as of now (October 2022), based on the `define-key`, which has been deprecated since Emacs 29.0.50.  And it also has a lousy indentation.  For these reasons, I refused to use these keywords.  Instead, I use `keymap-set` to do the key binding.

-   `:custom`
    
    I use `setq` everywhere.

-   `:custom-face`
    
    `:custom-face` keyword eval `custom-set-faces` before `require` feature.  Sometimes I need set faces after require, so `set-face-attribute` is more flexible.


#### Deferred loading conventions for `use-package`

[use-package.el](https://github.com/jwiegley/use-package) provides `:defer` keyword to control block to defer loading, which means eval the code of the `use-package` block after the specified feature has been loaded.  The `:defer` keyword is used to explicit emphasis defer loading.  In addition, the `:commands` and `:autoload` keywords will also implicitly defer loading by default.  So in this configuration, I will not use the `:defer` keyword explicitly in cases where it is not necessary.


#### Configure `use-package` how to expand

When using the `:hook` keyword to handle something to the hook, [use-package.el](https://github.com/jwiegley/use-package) will use an omitted hook name.  For example, `c-mode-hook` omitted as `c-mode`, `emacs-startup-hook` omitted as `emacs-startup`.  I'm a bit careless, and sometimes I mess things up.  So make [use-package.el](https://github.com/jwiegley/use-package) always use the proper hook name.

    (use-package use-package
      :defer t
      :init
      (setq use-package-hook-name-suffix nil))


### Get package from source

For packages that are not included in ELPA, I can only download them locally to use it.  But for such packages that already have version control, I manage them through quelpa.

    (use-package quelpa
      :ensure t
      :pin melpa
      :autoload quelpa-setup-p
      :init
      (setq quelpa-dir (format "%s%s/"
    			   (concat my-cache-directory "quelpa/")
    			   emacs-version))
    
      ;; Inhibit `quelpa' update MELPA's recipes every startup.
      (setq quelpa-update-melpa-p nil)
    
      ;; Inhibit `quelpa' auto-update.
      (setq quelpa-upgrade-p nil)
    
      ;; Inhibit `quelpa' upgrade itself.
      (setq quelpa-self-upgrade-p nil) 
    
      ;; Initialize `quelpa'.
      (quelpa-setup-p))


#### Extend keyword for Easy-to-use package configuration macro

I use a keyword named `:quelpa` in `use-package` macro, it implemented by [quelpa-use-package.el](https://github.com/quelpa/quelpa-use-package).

    (use-package quelpa-use-package
      :ensure t
      :pin melpa
      :after quelpa
      :init
      (require 'quelpa-use-package))


### Update Packages

Once I have installed the packages, a new question is, how do I update them?

The interactive approach is to call `list-packages`, press `U x` in the `*Package*` buffer, and then confirm according to the prompt to update all packages.  `U` means mark all upgradable packages, `x` means perform installation of marked packages.

It works, but&#x2026;not satisfied.

So I use [auto-package-update.el](https://github.com/rranelli/auto-package-update.el) to get further simplification and automation.

    (use-package quelpa
      :ensure t
      :pin melpa
      :commands
      (quelpa-upgrade-all
       quelpa-upgrade-all-maybe))
    
    (use-package auto-package-update
      :ensure t
      :pin melpa
      :commands
      (auto-package-update-maybe auto-package-update-now)
      :init
      (defun my-auto-package-update-now ()
        "Update installed Emacs packages.
    
    Update all packages that installed from `package' and `quelap'."
        (interactive)
        (auto-package-update-now)
        (quelpa-upgrade-all))
    
      (keymap-set ctl-c-e-map "u" 'my-auto-package-update-now)
    
      ;; Move `auto-package-update' status file.
      (setq auto-package-update-last-update-day-path
    	(expand-file-name "last-update" my-state-directory))
    
      ;; Delete residual old versions
      (setq auto-package-update-delete-old-versions t)
    
      ;; Do not bother me when updates have taken place.
      (setq auto-package-update-hide-results t)
    
      ;; Update installed packages at startup if there is an update
      ;; pending.
      (auto-package-update-maybe)
      (quelpa-upgrade-all-maybe))


<a id="orgdf38b20"></a>

## Inhibit saving customization to `user-init-file` by appending

Although I do not use the **Easy Customization Interface** of Emacs, some packages will use it somewhere I do not notice.

I want to keep the content of `user-init-file` and my configuration file not auto-generated, so I choose to save customizations somewhere other than my initialization file.

    (use-package cus-edit
      :defer t
      :preface
      (setq custom-file (expand-file-name "custom.el" my-state-directory))
      :when (file-exists-p custom-file)
      :init
      (load custom-file nil 'nomessage))


<a id="org0454e28"></a>

## Syncing to the shell's environment variables

For one reason or another, an Emacs instance begun from the desktop may miss some PATH set in the interactive shell.  Thus, it is challenging to use utilities installed by `cargo`.  I use an excellent package named [exec-path-from-shell.el](https://github.com/purcell/exec-path-from-shell) written by Steve Purcell to solve this problem, which will synchronize PATH from the shell.

    (use-package exec-path-from-shell
      :ensure t
      :pin melpa
      :when (memq window-system '(x pgtk))
      :hook
      (emacs-startup-hook . exec-path-from-shell-initialize))


<a id="org63b9c4e"></a>

## Intelligent garbage collection

Emacs uses a fixed threshold to trigger Garbage Collection by default, which is not flexible and may cause performance degradation by triggering Garbage Collection frequently.  I use a sneaky Garbage Collection strategy to minimize Garbage Collection interference with user activity.  It is provide by [gcmh.el](https://gitlab.com/koral/gcmh), the strategy will use a low Garbage Collection threshold when idling, and set a high threshold during normal.

    (use-package gcmh
      :ensure t
      :pin gnu
      :hook (emacs-startup-hook . gcmh-mode))


<a id="org3fbaa51"></a>

## UTF-8 everywhere

In the Linux world, there is a silent agreement that UTF-8 is the best encoding to use for Unicode.  So I use utf8 by default, unless I explicitly specify the encoding.

    (use-package mule-cmds
      :commands
      (set-language-environment
       set-default-coding-systems
       prefer-coding-system)
      :init
      (set-language-environment "utf-8")
      (set-default-coding-systems 'utf-8)
      (prefer-coding-system 'utf-8))


<a id="org4ab6e10"></a>

## Using Emacs as a server

When I invoke Emacs at other places, I want to share buffers, a command history, or additional information with the existing Emacs process.  So it is necessary to make Emacs start as a daemon.

    (use-package server
      :autoload server-running-p
      :config
      (setq server-auth-dir
    	(expand-file-name "server/" my-state-directory))
      :hook
      ;; Start server at startup.
      (emacs-startup-hook
       . (lambda ()
           (eval-when-compile (require 'server))
           (unless (server-running-p) (server-start)))))


<a id="orge73ddb6"></a>

## Resarting Emacs

When I change the configuration file, and in some other cases, I want to be able to restart Emacs instead of turning it off and on again.

    (use-package restart-emacs
      :ensure t
      :pin melpa
      :commands restart-emacs
      :init
      (keymap-set ctl-c-e-map "r" 'restart-emacs))


<a id="orgf4884dd"></a>

# Common Library

There are some shared libraries need to setup, mainly related to file of libraries saving.

[svg-lib.el](https://github.com/rougier/svg-lib) is used to provides a lot of icons in **svg** format, it will save the cache of icons to a directory under `user-emacs-directory`.  Make it follow my [File Convention](#orgdc8a2b0).

    (use-package svg-lib
      :ensure t
      :pin gnu
      :defer t
      :config
      (setq svg-lib-icons-dir
    	(expand-file-name "svg-lib/" my-cache-directory)))

[transient.el](https://github.com/magit/transient) implements abstraction involving a prefix command, infix arguments and suffix commands, it used to create interface for command-line program.  It will save levels, values, and history to a directory named *transient* under `user-emacs-directory`, make it follow my [File Convention](#orgdc8a2b0).

    (use-package transient
      :ensure t
      :pin melpa
      :defer t
      :config
      (setq transient-history-file
    	(expand-file-name "transient/history.el" my-state-directory)
    	transient-levels-file
    	(expand-file-name "transient/levels.el" my-state-directory)
    	transient-values-file
    	(expand-file-name "transient/values.el" my-state-directory)))


<a id="org1af613a"></a>

# Appearance

The default interface of Emacs is old-fashioned and old-school, and I'm a young person, so I want it to be more modern.  I used a more luminous Theme and Mode Line.


<a id="orgc8b9374"></a>

## Change the default frame layout

Emacs support work on graphical and non-graphical frames.  Frame layout will show a menu bar, tool bar, and vertical scroll bar by default.  The exception is the tool bar and vertical scroll bar are not displayed in the character-only frame.

Although the menu bar, tool bar, and scroll bar are practical, my operation flow is purely keyboard-based.  For me, they are rarely used and distract me, so I disable them.

Another unimportant reason is that they are initialized before eval the init file(after `before-init-hook`), which slow down Emacs startup.  Emacs will also get speedup by disabling them.

I handle anonymous functions to disable these bars to `emacs-startup~hook`.

    (use-package menu-bar
      :defer t
      :hook
      (emacs-startup-hook . (lambda () (menu-bar-mode -1))))
    
    (use-package tool-bar
      :defer t
      :hook
      (emacs-startup-hook . (lambda () (tool-bar-mode -1))))
    
    (use-package scroll-bar
      :defer t
      :hook
      (emacs-startup-hook . (lambda () (scroll-bar-mode -1))))


### Change the default frame layout during early initialization

If you notice that Emacs enable bars after `before-init-hook`, I disable bars at `emacs-startup-hook` in the previous paragraph.

This looks strange, so I turn them off by setting `default-frame-alist` in the `early-init-file`.

    ;; Add some essentia layout parameters of frame to preset values, and
    ;; ensure the minor modes corresponding to follow values are disabled.
    (push (cons 'menu-bar-lines nil) default-frame-alist)
    (push (cons 'tool-bar-lines nil) default-frame-alist)
    (push (cons 'vertical-scroll-bars nil) default-frame-alist)
    (push (cons 'horizontal-scroll-bars nil) default-frame-alist)
    (setq-default menu-bar-mode nil tool-bar-mode nil scroll-bar-mode nil)


<a id="orgddc7b95"></a>

## Change the default startup screen

Emacs will display **GNU Emacs** buffer that is not useful.  I use **scratch** buffer as default screen and clean the content of it and echo area.

    (use-package startup
      :no-require t
      :init
      (setq inhibit-startup-screen t
    	inhibit-startup-echo-area-message t
    	initial-scratch-message nil
    	initial-major-mode 'fundamental-mode))


<a id="org0050a87"></a>

## Resize pixelwise

I preferred resize frame and window pixelwise rather linewise.

    (use-package emacs
      :no-require t
      :init
      ;; Resize windows pixelwise.
      (setq window-resize-pixelwise t)
    
      ;; Resize frame pixelwise.
      (setq frame-resize-pixelwise t))


<a id="orgfb05468"></a>

## Accessible themes

Emacs' default theme leaves much to be desired: It does not look sleek and shiny, which usually leaves first-timers with a poor, shallow impression of the system.  Below I set up a theme that makes Emacs look accessible.

    (use-package modus-themes
      :ensure t
      :pin gnu
      :autoload
      (modus-themes-load-themes
       modus-themes-load-operandi
       modus-themes-load-vivendi)
      :init
      (setq modus-themes-bold-constructs t
    	modus-themes-italic-constructs t
    	modus-themes-subtle-line-numbers t
    	modus-themes-inhibit-reload t
    	modus-themes-fringes 'nil
    	modus-themes-mode-line '(borderless moody)
    	modus-themes-markup '(background italic)
    	modus-themes-links '(italic neutral-underline)
    	modus-themes-prompts '(intense bold)
    	modus-themes-headings
    	'((0 . (rainbow 1.5))
    	  (1 . (rainbow 1.3))
    	  (2 . (rainbow 1.2))
    	  (3 . (rainbow 1.1))
    	  (t . (rainbow))))
      (modus-themes-load-themes)
      :config
      (modus-themes-load-operandi)
      ;; Set more subtle line numbers.
      (when (member 'modus-operandi custom-enabled-themes)
        (let ((bg (face-attribute 'modus-themes-hl-line :background)))
          (set-face-attribute 'line-number-current-line
    			  nil
    			  :background bg))))

If you look at the themes built into the Emacs distribution, you will see that *modus-themes.el* are included in the emacs distribution.  But I always get the latest theme from ELPA.


### Show subtle line numbers

In Modus Themes, their background colors are not uniform when both `display-line-numbers-mode` and `hl-line-mode` are activated.  I have them displaying the same background color, which looks more subtle.

    (use-package modus-themes
      :ensure t
      :pin gnu
      :config
      (let ((bg (face-attribute 'modus-themes-hl-line :background)))
        (set-face-attribute 'line-number-current-line nil :background bg)))


<a id="orgca3ddff"></a>

## Tabs and Ribbons style Mode Line

The Mode Line is a part near the bottom of Emacs that gives information about the current buffer, such as encoding, buffer size, cursor position, major mode and minor mode.

I use [moody.el](https://github.com/tarsius/moody) to get a tabs and ribbons style mode line.  Its advantage is only making modest changes to the mode line, adding or replacing only a few elements.

    (use-package moody
      :ensure t
      :pin melpa
      :autoload
      (moody-replace-mode-line-buffer-identification
       moody-replace-vc-mode
       moody-replace-eldoc-minibuffer-message-function)
      :init
      ;; Replace elements of Mode Line.
      (moody-replace-mode-line-buffer-identification)
      (moody-replace-vc-mode)
      (moody-replace-eldoc-minibuffer-message-function))
    
    (use-package emacs
      :no-require t
      :init
      ;; Inhibit draw the underline at the same place as the descent line.
      (setq x-underline-at-descent-line t))


### Replace Minor Modes with a menu button

When a buffer activates so many minor modes, the Mode Line will show many names of minor modes.  It isn't charming because usually too long mode info messages can cause other information to be outside the scope of the frame.

I use [minions.el](https://github.com/tarsius/minions) to replace the minor modes list with a menu button that lists enabled minor modes.

    (use-package minions
      :ensure t
      :pin melpa
      :defines minions-prominent-modes
      :init
      ;; Show button as :)
      (setq minions-mode-line-lighter ":)"
    	minions-mode-line-delimiters '("" . ""))
      :hook (emacs-startup-hook . minions-mode))

`minions-prominent-modes` is used to show minor mode directly in the mode line.


<a id="org6675197"></a>

## Context Menu

Emacs support clicking the mouse button activates the menu whose contents depends on its surrounding context.  But it is disabled by default, sometimes it helps me, so I activated it at startup.

    (use-package mouse
      :hook (emacs-startup-hook . context-menu-mode))


<a id="orgcd698aa"></a>

## Smooth Scrolling

smooth scrolling, as its name indicates, is a feature that allows you to scroll smoothly. The default scrolling is a bit choppier, so I use `pixel-scroll-mode`.

    (use-package pixel-scroll
      :hook
      ((emacs-startup-hook . pixel-scroll-mode)
       (emacs-startup-hook . pixel-scroll-precision-mode)))


<a id="org1430af0"></a>

## Highlighting changes

Highlighting uncommitted changes on the fringe.

    (use-package diff-hl
      :ensure t
      :pin gnu
      :hook
      (find-file-hook . diff-hl-mode)
      (vc-dir-mode-hook  . diff-hl-dir-mode)
      (dired-mode-hook   . diff-hl-dired-mode)
      (diff-hl-mode-hook . diff-hl-flydiff-mode))


<a id="org315c874"></a>

# Completion

Emacs provides a **Completion** feature that fills in the rest of a name starting from an abbreviation for it.  I usually fill in two types of Emacs when I need to fill in.

-   **input completion**: completion of my input in the minibuffer
-   **text completion**: completion of words or abbreviations in a buffer.

When I fill in both types of content, I usually want them to have different interfaces.  I prefer input completions to show candidates in the minibuffer, while text completions provide a pop-up menu in place.

There are so many input completion packages in ELPA repositories, available for me to choose from [helm.el](https://github.com/emacs-helm/helm), [ivy.el](https://github.com/abo-abo/swiper), [selectrum.el](https://github.com/radian-software/selectrum), [vertico.el](https://github.com/minad/vertico), etc.

I have used all these packages for a long or short time.  I choose to use [vertico.el](https://github.com/minad/vertico) finally.  Because it achieves full compatibility with built-in Emacs completion commands and completion tables by reusing the built-in facilities system.

And for text completion, less choice, [company.el](https://github.com/company-mode/company-mode) and [corfu.el](https://github.com/minad/corfu).  Because [corfu.el](https://github.com/minad/corfu) relies exclusively on the standard Emacs completion API, [company.el](https://github.com/company-mode/company-mode) defines its own API for the backends.  So I use [corfu.el](https://github.com/minad/corfu) other than [company.el](https://github.com/company-mode/company-mode).


<a id="org824bfe7"></a>

## Allow recursive edit in minibuffer

By default, Emacs do not allow a new minibuffer in the current minibuffer.  It inconveniences many everyday tasks, such as viewing the documentation for a command before executing it or invoking the shell command `date` when calling `find-file` to create a file containing the current date.

I set `enable-recursive-minibuffers` to `t` to allow use recursive minibuffer.

    (use-package emacs
      :no-require t
      :init
      (setq enable-recursive-minibuffers t))


<a id="org8876acb"></a>

## Hide commands in M-x which do not work in the current mode

My Emacs contains thousands of commands, and I want to hide some commands that are not available in the current main mode to avoid interfering with my command completion.

In particular, note that this only works on Emacs after 28.0.50.

    (use-package simple
      :defer t
      :config
      (setq read-extended-command-predicate
    	#'command-completion-default-include-p))


<a id="org137ca8a"></a>

## Do not allow the cursor in the minibuffer prompt

Keep cursor outside of any `cursor-intangible` text property.

    (use-package emacs
      :no-require t
      :init
      (setq minibuffer-prompt-properties
    	'(read-only t cursor-intangible t face minibuffer-prompt)))
    
    (use-package cursor-sensor
      :hook
      (minibuffer-setup-hook . cursor-intangible-mode))


<a id="orga222e64"></a>

## Eye-catching indicator when calling `completing-read-multiple`

Add prompt indicator when calling `completing-read-multiple`, it will display `[CRM<separator>]`, e.g., `[CRM,]` if the separator is a comma.

    (use-package crm
      :defer t
      :config
      (define-advice completing-read-multiple
          (:filter-args (args) my-crm-indicator)
        "Parsing ARGS and add my prompt indicator.
    
    \(fn PROMPT TABLE &optional PREDICATE REQUIRE-MATCH INITIAL-INPUT HIST DEF INHERIT-INPUT-METHOD)"
        (cons (format "[CRM%s] %s"
    		  (replace-regexp-in-string
    		   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
    		   crm-separator)
    		  (car args))
    	  (cdr args))))


<a id="orge3f2474"></a>

## Match completion with space

Emacs supports the set of completion styles, with these three types enabled by default: `basic`, `emacs22`, and `partial-completion`.

They are corresponding to:

-   basic: completion of the prefix before point and the suffix after point.
-   emacs22: prefix completion that only operates on the text before point.
-   partial-completion: completion of multiple words, each one taken as a prefix.

For example, if there is a command `foo-bar`, it could be complete with these inputs:

-   `f_b`, complete by basic style
-   `f_r`, complete by emacs22 style
-   `f-_a`, complete by partial-completion style.

`_` is mean the position of cursor.

Although the default is sufficient, I would like to be allowed to complete the input with spaces as separators.  So  I use [orderless](https://github.com/oantolin/orderless), which provides a completion style named `orderless` that divides the pattern into space-separated components and match them.

    (use-package minibuffer
      :ensure orderless
      :pin gnu
      :init
      ;; Use orderless style get more flexible completions.
      (setq completion-styles '(substring orderless basic)
    	completion-category-overrides
    	'((file (styles basic partial-completion)))))

Additionally, enable `partial-completion` for file path expansion.  `partial-completion` is vital for file wildcard support.  Multiple files can be opened at once with `find-file` if the input has a wildcard.


<a id="org7283764"></a>

## Use vertical completion UI with [vertico.el](https://github.com/minad/vertico)

[vertico.el](https://github.com/minad/vertico) provides a minor mode named `vertico-mode` (Vertico Mode), which will wrap `completing-read` to set a vertical UI after activate it.

**Vertico Mode** is easy to use.  It just needs to be activated.  I handle it to `emacs-startup-hook` for auto-activated at startup.

    (use-package vertico
      :ensure t
      :pin gnu
      :hook
      (emacs-startup-hook . vertico-mode))

After activating Vertico Mode, it will display a fixed height minibuffer when completing.  I preferred to use an adjustable height based on the remaining candidates.

Another point needs to be tweaked.  When I go to the last candidate, I need help getting back to the top quickly. I enable cycling of `vertico-next` and `vertico-previous`.

    (use-package vertico
      :ensure t
      :pin gnu
      :config
      ;; Grow and shrink the Vertico minibuffer
      (setq vertico-resize t)
    
      ;; enable cycling for `vertico-next' and `vertico-previous'.
      (setq vertico-cycle t)
      :hook
      (emacs-startup-hook . vertico-mode))


### Ido-like directory navigation

`find-file` will open the current directory of files by default, so editing the path of `find-file` is a widespread operation, and by default, I can only delete one character at a time. That is too awful and will wear out my patience. [vertico.el](https://github.com/minad/vertico) has an extension named `vertico-directory`, which provides some commands to delete multiple characters in the path at once by word.  I bind these commands to backspace.

    (use-package vertico-directory
      :ensure vertico
      :pin gnu
      :after vertico
      :autoload
      (vertico-directory-enter
       vertico-directory-delete-char
       vertico-directory-delete-word)
      :init
      (keymap-set vertico-map "<return>" 'vertico-directory-enter)
      (keymap-set vertico-map "<backspace>" 'vertico-directory-delete-char)
      (keymap-set vertico-map "M-<backspace>" 'vertico-directory-delete-word)
      :hook
      ;; Tidy shadowed file names.
      (rfn-eshadow-update-overlay-hook . vertico-directory-tidy))


### Use Mouse select candidate

I occasionally use the mouse, and this is why I want to use the mouse to select the candidates.

    (use-package vertico-mouse
      :ensure vertico
      :pin gnu
      :after vertico
      :hook
      (vertico-mode-hook . vertico-mouse-mode))


<a id="orgee8e181"></a>

## Completion Overlay Region FUnction

I use [corfu.el](https://github.com/minad/corfu) to pop up the completion of text at a point.

    (use-package corfu
      :ensure t
      :pin gnu
      :commands corfu-mode)

Because I avoid using too many global modes and only complete text when programming, `corfu-mode` is handled in the hook of the programming major mode.  As you see, I autoload the command and do not activate it.

**Corfu Mode** works as a wrapper of `complete-symbol`, so I need press `C-M-i` to do complete.  Let it pop up the completion menu automatically.

    (use-package corfu
      :ensure t
      :pin gnu
      :commands corfu-mode
      :config
      (setq corfu-auto t
    	corfu-quit-no-match 'separator))

**Corfu Mode** jumping between candidates with `corfu-perivous` and `corfu-next`.  When I got to the last candidate, I wanted the next one to return to the first one, so I activated cycling for `corfu-perivous` and `corfu-next`.

    (use-package corfu
      :ensure t
      :pin gnu
      :commands corfu-mode
      :config
      ;; Enable auto completion
      (setq corfu-auto t
    	corfu-quit-no-match 'separator)
    
      ;; Enable cycling for `corfu-next' and `corfu-previous'.
      (setq corfu-cycle t))

When I selected an incorrect candidate and quit completion, it has been fill in the point.  It will waste my time to delete it, so I prevent candidate preselection.

    (use-package corfu
      :ensure t
      :pin gnu
      :commands corfu-mode
      :config
      ;; Enable auto completion
      (setq corfu-auto t
    	corfu-quit-no-match 'separator)
    
      ;; Enable cycling for `corfu-next' and `corfu-previous'.
      (setq corfu-cycle t)
    
      ;; Disable candidate preselection.
      (setq corfu-preselect-first nil))


### Tab style completion

Complete text by just pressing the `TAB` key is a more efficient operation.

    (use-package corfu
      :defer t
      :config
      ;; TAB-style keybindings.
      (keymap-set corfu-map "<tab>" 'corfu-next)
      (keymap-set corfu-map "<backtab>" 'corfu-previous))


### Work in character-only frame

[corfu.el](https://github.com/minad/corfu) was created on the top of `posframe`, so it only works in the graphical frame.  Sometimes I will use Emacs in the character-only frame (terminal).  I need help to complete the text at this time.  Unsurprisingly, the powerhouse Emacs community already has a ready-made package named [corfu-terminal.el](https://codeberg.org/akib/emacs-corfu-terminal).

This package provides a global minor mode named `corfu-terminal-mode` to replace `posframe` with `popup`.  The only drawback is that it is global, I rewrite it to a minor mode.  The only drawback is that it is global.  I have rewritten it to a non-global minor mode.

    (use-package corfu
      :ensure t
      :pin gnu
      :autoload
      (corfu--popup-show
       corfu--popup-hide
       corfu--popup-support-p))
    
    (use-package corfu-terminal
      :ensure t
      :pin nongnu
      :unless (display-graphic-p)
      :autoload (corfu-terminal--popup-show
    	     corfu-terminal--popup-hide
    	     corfu-terminal--popup-support-p)
      :preface
      (define-minor-mode my-corfu-terminal-mode
        "Corfu popup on terminal."
        :global nil
        :group 'corfu-terminal
        (if my-corfu-terminal-mode
    	(progn
    	  (advice-add #'corfu--popup-show :around
    		      #'corfu-terminal--popup-show)
    	  (advice-add #'corfu--popup-hide :around
    		      #'corfu-terminal--popup-hide)
    	  (advice-add #'corfu--popup-support-p :override
    		      #'corfu-terminal--popup-support-p))
          (advice-remove #'corfu--popup-show #'corfu-terminal--popup-show)
          (advice-remove #'corfu--popup-hide #'corfu-terminal--popup-hide)
          (advice-remove #'corfu--popup-support-p
    		     #'corfu-terminal--popup-support-p)))
      :hook
      (corfu-mode-hook . my-corfu-terminal-mode))

Now I can complete text in terminal.


### Remember selected candidates and to improve sorting

When I write code in an actual project, some functions or variables need to be completed more often than others. So I would like **Corfu Mode** can do this based on the frequency of use.

    (use-package corfu-history
      :ensure corfu
      :pin gnu
      :hook
      (corfu-mode-hook . corfu-history-mode))


### Show documentation when select candidate

[corfu-doc.el](https://github.com/galeo/corfu-doc) provides a minor mode that display a documentation popup for completion candidate when using **Corfu Mode**, it helped me tremendously to complete the text.

    (use-package corfu-doc
      :ensure t
      :pin melpa
      :after corfu
      :config
      (keymap-set corfu-map "M-d" 'corfu-doc-toggle)
      (keymap-set corfu-map "M-n" 'corfu-doc-scroll-up)
      (keymap-set corfu-map "M-p" 'corfu-doc-scroll-down)
      :hook (corfu-mode-hook . corfu-doc-mode))


### Completion in the miniubuffer

I frequently use `eval-expression` (`M-:`) to temporary eval code, make it can be complete with **Corfu Mode**.

    (use-package corfu
      :ensure t
      :pin gnu
      :preface
      (defun my-corfu-in-minibuffer ()
        "Enable Corfu in the minibuffer if `completion-at-point' is bound."
        (when (where-is-internal #'completion-at-point
    			     (list (current-local-map)))
          (corfu-mode +1)))
      :hook
      (minibuffer-setup-hook . my-corfu-in-minibuffer))


### Beautify with identifiable icon

Some functions and variables are named similarly when writing code, and I need to distinguish them visually.  I use [kind-icon.el](https://github.com/jdtsmith/kind-icon) to add some colorful icon before candidates.

    (use-package corfu
      :ensure t
      :pin gnu
      :defines corfu-margin-formatters)
    
    (use-package kind-icon
      :ensure t
      :pin gnu
      :after corfu
      :autoload kind-icon-margin-formatter
      :init
      (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter)
      :config
      ;; Compute blended backgrounds correctly.
      (setq kind-icon-default-face 'corfu-default))


<a id="orgde18eed"></a>

## Show mariginalia of completions

I use [marginalia.el](https://github.com/minad/marginalia) get more helpful infomation of completion.

> [Marginalia](https://en.wikipedia.org/wiki/Marginalia) are marks or annotations placed at the margin of the page of a book or in this case helpful colorful annotations placed at the margin of the minibuffer for your completion candidates.<sup><a id="fnr.1" class="footref" href="#fn.1" role="doc-backlink">1</a></sup>

    (use-package marginalia
      :ensure t
      :pin gnu
      :config
      ; Make marginalia align to right.
      (setq marginalia-align 'right)
      :hook
      (emacs-startup-hook . marginalia-mode))


<a id="org25d6488"></a>

# Actions in Emacs

For me, there are some commands in Emacs that I consider to actions.  Such as opening the URL in a browser, switching to other buffers, searching for something, and operating them.  So I need quickly find these actions and enhance them for me.


<a id="org721c5db"></a>

## Save actions history

When I do some actions and quit Emacs later, the history of actions will be lost.  I have to retype everything, and it upsets me.  So I activate `savehist-mode` to persist everything typed in the minibuffer.

    (use-package savehist
      :config
      (setq savehist-file (expand-file-name "hist.el" my-state-directory))
      ;; Auto delete duplicated history.
      (setq history-delete-duplicates t)
      :hook (emacs-startup-hook . savehist-mode))


<a id="org6e05076"></a>

## Switch buffer with preview

At this point, in addition to memorizing their differences, I'd like to preview them briefly before switching between them.  I use a command named `consult-buffer` provide by [consult.el](https://github.com/minad/consult) instead of the default `switch-to-buffer`.

    (use-package consult
      :ensure t
      :pin gnu
      :commands consult-buffer
      :init
      (keymap-substitute global-map 'switch-to-buffer 'consult-buffer))


<a id="org7abb680"></a>

## Avaiable actions menu

In one place, I may execute some commands.  Rather than binding all the relevant commands to a series of keystrokes and memorizing it, I prefer to have an interactive menu pop up for me to select it.  So I use [embark.el](https://github.com/oantolin/embark) to show actions (commands) relevant to the target around point.  It provides a context menu of commands.

    (use-package embark
      :ensure t
      :pin gnu
      :commands
      (embark-act
       embark-dwim)
      :init
      (keymap-set global-map "C-;" 'embark-act)
      (keymap-set global-map "C-'" 'embark-dwim))

This will show all available actions on the current point by pressing `C-;`,  and call the last action again by pressing `C-'`.


### Hide Mode Line in `embark` buffers

When I use `embark-collect` and `embark-live`, I hope hide the Mode Line.

    (use-package window
      :defer t
      :init
      ;; Hide the mode line of the Embark live/completions buffers
      (add-to-list 'display-buffer-alist
    	       '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
    		 nil
    		 (window-parameters (mode-line-format . none)))))


### Collect with preview

Automatic preview at point in the Embark Collect buffer.

    (use-package consult
      :ensure t
      :pin gnu
      :commands consult-preview-at-point-mode)
    
    (use-package embark
      :ensure embark-consult
      :commands embark-collect-mode
      :hook
      (embark-collect-mode-hook . consult-preview-at-point-mode))


<a id="org44b7cc2"></a>

## Help Actions

When I get confused about using Emacs, I use a series of help commands built into Emacs to solve my confusion.  But &#x2026; it is not enough.  So I need some better third-party help commands to help me learn about more things.


### Show avaiable bindings

Sometimes I need get the description of `embark-act` bindings.

    (use-package embark
      :ensure t
      :pin melpa
      :commands embark-bindings
      :init
      (keymap-set help-map "C-b" 'embark-bindings))


### Describe keymap

Emacs has a built-in command named `describe-keymap`, but it is not bind to any key by default.  It is useful, I bind it to `C-h C-k`.

    (use-package help-fns
      :commands describe-keymap
      :init
      (keymap-set help-map "C-k" 'describe-keymap))


### Search available bindings

Some people use [which-key.el](https://github.com/justbur/emacs-which-key), which provides a popup buffer to display all available key bindings.  I used to use it too.  Although it shows bindings as much as possible, I need help finding a command when I have many bindings.  I use `embark-prefix-help-command` provides by [embark.el](https://github.com/oantolin/embark) to search binding with a `completing-read` interface.

    (use-package embark
      :ensure t
      :pin gnu
      :commands embark-prefix-help-command)
    
    (use-package emacs
      :no-require t
      :init
      (setq prefix-help-command #'embark-prefix-help-command))


<a id="orgc123dc4"></a>

## Adjust scrolling distance

Emacs will scroll more than half of the screen when I am at the bottom of the buffer and want to continue down.  It causes the focus of my eyes to leave the bottom of the buffer.

    (use-package emacs
      :no-require t
      :init
      ;; Automatically adjust ‘window-vscroll’ to view tall lines.
      (setq auto-window-vscroll nil)
    
      ;; Accelerate scrolling operations
      (setq fast-but-imprecise-scrolling t)
    
      ;; Kell current position after screen scroll.
      (setq scroll-preserve-screen-position t)
    
      ;; When at the bottom or top of the buffer, move only one line.
      (setq scroll-margin 0 scroll-conservatively 101)
    
      ;; Horizontally scrolling threshold.
      (setq hscroll-margin 2 hscroll-step 1))


<a id="org78c2efe"></a>

## Mouse actions

As with most graphics software, Emacs has mouse interactions, some of which are difficult to use and inadequate.  I've tweek them to make the interactions feel more subtle.


<a id="org6dd1e1f"></a>

## Drag and Drop

> In computer graphical user interfaces, drag and drop is a pointing device gesture in which the user selects a virtual object by "grabbing" it and dragging it to a different location or onto another virtual object.<sup><a id="fnr.2" class="footref" href="#fn.2" role="doc-backlink">2</a></sup>

I mostly drag and drop text with mouse.

    (use-package mouse
      :config
      (setq mouse-drag-and-drop-region 'control))


<a id="orgecea876"></a>

## Mouse yank at point

Mouse yank commands yank at point instead of at click.

    (use-package mouse
      :config
      (setq mouse-yank-at-point t))


<a id="org02e8f2e"></a>

## Search actions

I use [consult.el](https://github.com/minad/consult) to improve search commands with preview.

    (use-package consult
      :ensure t
      :pin gnu
      :commands
      (consult-find
       consult-grep
       consult-keep-lines
       consult-line
       consult-ripgrep
       consult-locate
       consult-git-grep
       consult-focus-lines
       consult-line-multi
       consult-multi-occur)
      :init
      (keymap-set search-map "f" 'consult-find)
      (keymap-set search-map "g" 'consult-grep)
      (keymap-set search-map "k" 'consult-keep-lines)
      (keymap-set search-map "l" 'consult-line)
      (keymap-set search-map "r" 'consult-ripgrep)
      (keymap-set search-map "M-f" 'consult-locate)
      (keymap-set search-map "M-g" 'consult-git-grep)
      (keymap-set search-map "M-k" 'consult-focus-lines)
      (keymap-set search-map "M-l" 'consult-line-multi)
      (keymap-set search-map "M-o" 'consult-multi-occur))


<a id="org47fda81"></a>

## Goto actions

I also use [consult.el](https://github.com/minad/consult) to improve goto commands with preview.

    (use-package consult
      :ensure t
      :pin gnu
      :commands
      (consult-global-mark
       consult-goto-line
       consult-imenu
       consult-imenu-multi
       consult-mark
       consult-outline)
      :init
      (keymap-substitute goto-map 'goto-line 'consult-goto-line)
      (keymap-substitute goto-map 'imenu 'consult-imenu)
      (keymap-set goto-map "m" 'consult-mark)
      (keymap-set goto-map "o" 'consult-outline)
      (keymap-set goto-map "M-i" 'consult-imenu-multi)
      (keymap-set goto-map "M-m" 'consult-global-mark))


<a id="orge68b1b3"></a>

# Use Emacs as general text editor

Emacs is an integrated computing environment, but it also has some features of the modern text editor.  Such as auto-saving, backup, and remote accessing.

And Emacs is a visual editor.  That means that I have a representation of my entire document on your screen, and I can move around freely, editing any part of the document I wish.  So I've also enhanced some visual features of the text.


<a id="orge470f69"></a>

## Automatic Backups

By default, Emacs saves backup files —those ending in ~— in the current directory, thereby cluttering it up.  Let's place them in a directory follow my [File Convention](#orgdc8a2b0).

I want to keep multiple versions of my backup files to help keep my sanity.  Emacs allow saving an unlimited number of backups, but keeping a backup of five versions is appropriate.

    (use-package files
      :defer t
      :config
      ;; Create backup files for each modified file on saving, and enable
      ;; make multiple numbered backup files.
      (setq make-backup-files t
    	version-control t)
    
      ;; Make backup files using the old file replication.
      (setq backup-by-copying t)
    
      ;; Adjust the threshold for automatic deletion of versioned backup
      ;; files.
      (setq delete-old-versions t
    	kept-old-versions 5
    	kept-new-versions 5)
    
      ;; Prevent create backup files in-place, alternative create them in
      ;; `my-data-directory'.
      (setq backup-directory-alist
    	(append `(("."
    		   .
    		   ,(expand-file-name "backup/" my-data-directory)))
    		backup-directory-alist)))

You may have noticed that I set `backup-by-copying` to `t`, which makes Emacs create backups by copying from the original file.  This differs from the default method, which creates backups by renaming the original file.


<a id="org8f41497"></a>

## Automatic Saving

By default, Emacs automatically saves your changes to a file intermittently.  If anything should happen, you can recover a file with `M-x recover-file`.

And Emacs will create the auto-saved file with a name appending **#** to the front and rear of the visited file name in place.  I want to keep the directory clean, and follow my [File Convention](#orgdc8a2b0).  So I push my customized transform for rule of making auto-save file name to `auto-save-file-name-transforms`.

    (use-package files
      :defer t
      :config
      ;; Enable Auto-saving.
      (setq auto-save-default t)
    
      ;; Disable the message of auto-saving.
      (setq auto-save-no-message t)
    
      ;; Don't auto-disable auto-save after deleting big chunks.  This
      ;; defeats the purpose of a failsafe.
      (setq auto-save-include-big-deletions t)
    
      ;; Change default transforms to apply to buffer file name before
      ;; making auto-save file name.
      (setq auto-save-file-name-transforms
    	(append `((".*"
    		   ,(expand-file-name "auto-save/" my-data-directory)
    		   t))
    		auto-save-file-name-transforms)))

Deleting a substantial portion of the text disables auto-save in the buffer by default.  Thus, if I make many changes and my computer suddenly fails, all my changes will be lost, so save all changes anyway.  For always failsafe to take precedence, I have to set `auto-save-include-big-deletions` to `t`.

After auto-saved files have been created, Emacs will save paths of all auto-saved files to a file named the value of `auto-save-list-file-name`.  Make it follow my [File Convention](#orgdc8a2b0).

    (use-package startup
      :no-require t
      :init
      (setq auto-save-list-file-prefix
    	(expand-file-name "auto-save/session/" my-data-directory)))


<a id="org21a5d81"></a>

## Recent Files

Same as criminals always return to the scene of the crime, I also reopen file from time to time when I am done hacking code or writing articles.  Remembering the path to the file and recalling `find-file` is tedious.  Fortunately, Emacs provides **Recentf Mode** to facilitate our return to the crime scene.  It will maintain and persist a list of recent files, I call `recentf-open` to get this list after activate **Recentf Mode**.

    (use-package recentf
      :hook (emacs-startup-hook . recentf-mode))

The list of recented files will store in `user-emacs-directory` by default, make it follow my [File Convention](#orgdc8a2b0).  In addition, I use an incredible command named `consult-recent-file` provided by [consult.el](https://github.com/minad/consult) to give a preview of recent files.

    (use-package consult
      :ensure t
      :pin gnu
      :commands (consult-recent-file))
    
    (use-package recentf
      :init
      (setq recentf-save-file
    	(expand-file-name "recent.el" my-state-directory))
      (keymap-set ctl-c-f-map "r" 'consult-recent-file)
      :hook (emacs-startup-hook . recentf-mode))

**Recentf Mode** will print some message in echo area at Emacs startup, like:

    Loading /org/brsvh/bsc/.local/state/emacs/recent.el (source)...done
    Cleaning up the recentf list...done (0 removed)

It isn't enjoyable and doesn't help, so I created some advice to stop it from appearing.

    (use-package consult
      :ensure t
      :pin gnu
      :commands (consult-recent-file))
    
    (use-package recentf
      :init
      (setq recentf-save-file
    	(expand-file-name "recent.el" my-state-directory))
      (keymap-set ctl-c-f-map "r" 'consult-recent-file)
      :config
      (define-advice recentf-load-list
          (:around (fn &rest args) silence-message)
        "Silencing load message."
        (cl-letf (((symbol-function #'message) #'ignore))
          (apply fn args)))
    
      (define-advice recentf-cleanup
          (:around (fn &rest args) silence-message)
        "Silencing clean up message."
        (cl-letf (((symbol-function #'message) #'ignore))
          (apply fn args)))
      :hook (emacs-startup-hook . recentf-mode))


<a id="orgd5d6030"></a>

## Show basic information of buffer

The basic capabilities of the text editor should include showing basic information of buffer.  For me, they are the number of current line and column, the size of current buffer(file).

    (use-package simple
      :hook
      ((emacs-startup-hook . column-number-mode)
       (emacs-startup-hook . line-number-mode)
       (emacs-startup-hook . size-indication-mode)))


<a id="org7d35ff2"></a>

## Save the last opened place

After I modified a file, I saved and closed it.  Emacs will go to the top line of the file when next opened.  After I modified a file, I saved and closed it.  Emacs will go to the top line of the file when next opened.  I'm always trying to find where the last revision went.  To avoid this hassle, I used Save Place Mode to save the last place of the file before closing it.

    (use-package saveplace
      :config
      (setq save-place-file
    	(expand-file-name "place.el" my-state-directory))
      :hook (emacs-startup-hook . save-place-mode))

Make date storage of *saveplace.el* follow my [File Convention](#orgdc8a2b0).


<a id="org110d415"></a>

## Replace region with new content

Emacs will append the new content to the marked region by default, which goes against operational intuition.

    (use-package delsel
      :hook
      ;; Replace content in marked region with new text.
      (emacs-startup-hook . delete-selection-mode))


<a id="org82128d6"></a>

## Display file name with forward style

When I opened two files that have same name, the buffers of these files will append parent directory name with brackets.  It looks different form the Unix/Linux file path, I preferred to use forward style.

    (use-package uniquify
      :init
      ;; Use forward style buffer name.
      (setq uniquify-buffer-name-style 'forward))


<a id="orgb28e512"></a>

## Kill ring and Clipboard

The **kill ring** is a list of blocks of text that were previously killed. And the **clipboard** is the facility that most graphical applications use for *cutting and pasting*.  I would like to make the kill ring of Emacs and the clipboard of system use the same content.  So I need set Emacs save kill ring to clipboard after quit.

    (use-package simple
      :config
      ;; Do not saves duplicates in kill-ring
      (setq kill-do-not-save-duplicates t)
    
      ;; Save clipboard after quit.
      (setq save-interprogram-paste-before-kill t))

I set `kill-do-not-save-duplicates` to `t`, it will remove duplicated content in the kill ring.


<a id="org5e8dae5"></a>

# File Management

Emacs can be used as a file manager, in addition to local file management, there is also rich remote protocol support.  I use Emacs as file management client to operate files, and version control.


<a id="orgad7b77e"></a>

## Version Control

**Version Control** is is a class of systems responsible for managing changes to computer programs, documents, large web sites, or other collections of information.

Widely used version control software includes **Git**, **Subversion**, **Mercurial**, and etc.


#### Git support

[magit.el](https://github.com/magit/magit) is a complete text-based user interface to [Git](https://git-scm.com/), it is the world's best Git client.

    (use-package magit
      :ensure t
      :pin nongnu
      :commands magit-dispatch
      :init
      (setq magit-define-global-key-bindings nil)
      (keymap-set ctl-c-v-map "g" 'magit-dispatch))


#### Update highlighting view after `magit` commit

`diff-hl` do not refresh fringe after changes have been committed with `magit`, handle refresh functions of `diff-hl` in related `magit` hooks.

    (use-package magit
      :ensure t
      :pin nongnu
      :defines
      (magit-pre-refresh-hook
       magit-post-refresh-hook))
    
    (use-package diff-hl
      :ensure t
      :pin gnu
      :hook
      ((magit-pre-refresh-hook . diff-hl-magit-pre-refresh)
       (magit-post-refresh-hook . diff-hl-magit-post-refresh)))


<a id="org491c523"></a>

# Life with `org-mode`

My life can be seen as a combination of mechanical tasks, the planning and finishing of which make up my day.  It's easier to estimate how long a task takes if I keep track of time spent by *clocking in and out* of tasks.

All my **Org** files stored in the *org* directory under the `HOME` directory.

    (use-package org
      :ensure t
      :pin gnu
      :defer t
      :config
      (setq org-directory "~/org"))


<a id="orga717209"></a>

## Using `org-mode` as a Day Planner

I started realizing my [GTD methodology](https://en.wikipedia.org/wiki/Getting_Things_Done) from [Nicolas Pretton's post](https://emacs.cafe/emacs/orgmode/gtd/2017/06/30/orgmode-gtd.html) as a starting point.  I call my GTD methodology as **Day Planner**.

My workflow of the day planner with the following steps:

1.  Mindlessly capture and collect tasks.
2.  Schedule and archive tasks at the end, or start, of the day.
3.  Document in detail the tasks I encounter problems or ideas while doing them.
4.  Repeat!

I use *org-agenda.el* to show my tasks by time or category and bind related commands to my customized global key map.

    (use-package org-agenda
      :ensure org
      :pin gnu
      :commands org-agenda
      :init
      (keymap-set ctl-c-o-map "a" 'org-agenda))


<a id="orgd4cda7a"></a>

## Files of Day Planner

I split all my tasks in four separate files:

-   *~/org/agenda/inbox.org*: where I collect everything;
-   *~/org/agenda/gtd.org*: where I put all my projects;
-   *~/org/agenda/someday.org*: All inactive tasks that I might do at some point in the future, but don’t want to see all the time;
-   *~/org/agenda/tickler.org*: I put entries in this file with a timestamp to get reminded at the right moment.

And it is necessary to add these files to the agenda file.

    (use-package org-agenda
      :ensure org
      :pin gnu
      :defer t
      :config
      (add-to-list 'org-agenda-files "~/org/agenda/inbox.org")
      (add-to-list 'org-agenda-files "~/org/agenda/gtd.org")
      (add-to-list 'org-agenda-files "~/org/agenda/tickler.org"))

My inbox is then processed and emptied daily.  When processing the inbox, I refile each entry that is actionable and belongs to a project using `C-c C-w`, moving the entry to the appropriate place.  If need be, I create a new project out of it.  These are rules for refile entry.

    (use-package org-refile
      :ensure org
      :pin gnu
      :defer t
      :config
      (add-to-list 'org-refile-targets
    	       '("~/org/agenda/gtd.org" :maxlevel . 3))
      (add-to-list 'org-refile-targets
    	       '("~/org/agenda/someday.org" :level . 1))
      (add-to-list 'org-refile-targets
    	       '("~/org/agenda/tickler.org" :maxlevel . 2)))


<a id="org21ef27a"></a>

## Capture and collect tasks

As mentioned above, I use the inbox as a pre-processing place for all tasks.  So I needed a quick way to capture tasks to the inbox, and *org-capture.el* provided it.

    (use-package org-capture
      :ensure org
      :pin gnu
      :commands org-capture
      :init
      (keymap-set ctl-c-o-map "c" 'org-capture)
      :config
      ;; Task.
      (add-to-list 'org-capture-templates
    	       '("t"
    		 "Todo [inbox]"
    		 entry
    		 (file+headline "~/org/agenda/inbox.org" "Tasks"))
    	       'append)
    
      ;; Tickler
      (add-to-list 'org-capture-templates
    	       '("T"
    		 "Tickler"
    		 entry
    		 (file+headline "~/org/agenda/tickler.org" "Tickler"))
    	       'append))


<a id="orge86410b"></a>

# Prose

Emacs is a writer's best friend, the rich extensions from the Emacs community and the extensibility of Emacs itself offer endless possibilities for writing.


<a id="orge584033"></a>

## Generic writing experience improvements for text of Prose

I've thought about what I need from Emacs to support me while working on my literary creations.  They just include **Text Wrap** and **Spell Check** now.

I always use soft wrap when I need to wrap text, and it is **Visual Line Mode** in Emacs to provide this support.

    (use-package simple
      :hook
      (text-mode-hook . hl-line-mode))
    
    (use-package hl-line
      :hook
      (text-mode-hook . visual-line-mode))

As you see, I also need highlight current line when wirte prose.

There are multiple choices for Spell Check in Emacs, *ispell.el* or *flyspell.el*.

`ispell-word` is bind to `M-$` by default, it is configure free and I don't need setup it.  So I just handle `flyspell-mode` to `text-mode-hook`, it will be available in other writing modes that inherit from `text-mode`.

    (use-package flyspell
      :hook
      (text-mode-hook . flyspell-mode))


### Wrap Multi-language Text

I am a Chinese native speaker, so I frequently write English and Chinese at the same time.  They will have strange line breaks wrap in Emacs, so it is necessary to set `word-wrap-by-category` to get correct word wrap for me.  It is useful for Korean and Japanese too.

    (use-package simple
      :config
      (setq word-wrap-by-category t))


<a id="orge9afac8"></a>

## Distraction-free writing

Writing takes total concentration to produce creative prose.  Distractions are the natural enemy of concentration.  While my computer is my most important writing tool, it can also be a source of distractions.

Distraction-free writing means that the computer screen is free of clutter and, just like an old typewriter, only shows the text is working on.

[olivetti.el](https://github.com/rnkn/olivetti) is a simple Emacs minor mode that facilities distraction-free writing.  I use it when need full concentration writing in other marjor modes.

    (use-package olivetti
      :ensure t
      :pin melpa
      :commands olivetti-mode)


<a id="org8e8aafd"></a>

## Write article with `org-mode`

**Org Mode** is a GNU Emacs major mode for org file format editing, and org file format is markup text language with rich features.  Most of my plain text files are written in org file format.  I export them to other file formats via `ox`.  Let's tweak it now.


### Beautify Org Mode

Org mode needs some tweaks to remove some clutter from the screen.  We can change the alignment, change the way fonts of rich text and special characters are displayed and preview images by default.

    (use-package org
      :ensure t
      :pin gnu
      :defer t
      :config
      ;; Show entities as UTF8 characters.
      (setq org-pretty-entities t)
    
      ;;Hide emphasis markers 
      (setq org-hide-emphasis-markers t)
    
      ;; Show inline image.
      (setq org-startup-with-inline-images t))

Hiding emphasis markers is that rich text becomes hard to edit because it is unclear whether your cursor is on the marker or the first or last character.  The [org-appear.el](https://github.com/awth13/org-appear) helps by displaying the markers while the cursor is on a rich text word.

When I'm editing the headline, I adjust the headline level with the `tab` key. So `C-a` return to before leading star is not appropriate, I always expect to change the first word of the headline instead of changing the level.  Set `org-special-ctrl-a/e` to move cursor to the beginning of headline when press `C-a`.  The same is applied to `C-e`, which moves to the end of the headline when editing a folded block.

    (use-package org
      :ensure t
      :pin gnu
      :defer t
      :config
      (setq org-special-ctrl-a/e t))

    (use-package org-appear
      :ensure t
      :pin melpa
      :config
      (setq org-appear-autolinks t)
      :hook
      (org-mode-hook . org-appear-mode))

The asterisk characters used by Org mode to indicate heading levels are practical but look a bit ugly.  The previous configuration already removed the leading stars to reduce clutter.  Next I will replace these symbols with some nice icons.

The [org-modern.el](https://github.com/minad/org-modern) improves the look of Org Mode headings by replacing the asterisk symbols with nicer looking circles.  The package also enhances the looks of plain lists, todo items, and tables.

    (use-package org-modern
      :ensure t
      :pin gnu
      :config
      ;; Set headline stars use zero width space.
      (setq org-modern-star '("\u200b"))
    
      ;; Hide all leading stars.
      (setq org-modern-hide-stars t)
      :hook
      (org-mode-hook . org-modern-mode))

Let tags align to the end of headline, it will give a better display.

    (use-package org
      :ensure t
      :pin gnu
      :defer t
      :config
      ;; Tags align
      (setq org-auto-align-tags nil
    	org-tags-column 0))

**Org Mode** have some export option for export Org file to other file format.  But I need hide them when writing.

    (use-package org
      :ensure t
      :pin gnu
      :config
      (setq org-hidden-keywords '(title
    			      author
    			      date
    			      email
    			      tags
    			      options
    			      export_file_name)))


### Show Outline Tree

**Org Mode** supports jumping between sections and can also use `occur` to match the headline.   However, I still need an intuitive idea of the structure of the current file when writing so-long files.  I use a command `consult-org-heading` provide by [consult.el](https://github.com/minad/consult) to preview heading and jump into it.

    (use-package consult
      :ensure t
      :pin gnu
      :commands consult-org-heading)
    
    (use-package org
      :ensure t
      :pin gnu
      :defer t
      :config
      (keymap-set org-mode-map "M-s M-h" 'consult-org-heading))


### Distraction-free Org writing

Full concentration to write Org file.

    (use-package olivetti
      :ensure t
      :pin melpa
      :hook
      (org-mode-hook . olivetti-mode))


### Export to other file formats

At some point I want to export my **Org** file to other file formats, such as publish them on the web, or share them with people not using **Org**.  **Org Mode** use *ox.el* to support for conversion to other files.  But it only activate supports of **HTML** file, **ASCII** file, **Plain** text, **ODT** (Open Document Text) file by default, and I want to get more types support.


#### Markdown

**Markdown** is a popular lightweight markup language for creating formatted text.  It is the default document format that most people use.  So I need easily convert to it to share with the people who use it.

Require *ox-md.el* to auto extended Markdown support of  `org-export-dispatch`.

    (use-package ox-md
      :ensure org
      :pin gnu
      :after org)


<a id="org43a02a3"></a>

## Write article with LaTeX

[LaTeX](https://www.latex-project.org/) is a powerful, elegant, and rich ecological document preparation system.  I use it to create formal, polished, high-quality articles and presentations.  I started using Emacs many years ago because Emacs had the best LaTex support.  Of course, I mean the [auctex.el](https://www.gnu.org/software/auctex/).  I use its default workflow to compile and preview my LaTeX document.


### Work with AUCTeX

As I mentioned in this file, I am a Chinese native speaker.  So I often use LaTeX to write Chinese documents.  Actually writing Chinese documents using only LaTeX is tedious, and I used to get lost in the [CTeX](http://www.ctex.org/HomePage) configuration.  And In recent years, I have been using [XeTeX](https://tug.org/xetex/)(and XeLaTeX), which is a TeX typesetting engine using Unicode and supporting modern font technologies.  And then I can write Chinese documentation quickly with very little configuration.

Unfortunately, *auctex.el* doesn't support compiling XeLaTex by default.  But I can simply add XeLaTeX compile command to `TeX-command-list` to support it.

    (use-package tex-site
      :ensure auctex
      :pin gnu
      :defer t)
    
    (use-package tex
      :ensure auctex
      :pin gnu
      :defer t
      :config
      (add-to-list 'TeX-command-list
    	       '("XeLaTeX"
    		 "xelatex -interaction=nonstopmode %s"
    		 TeX-run-command
    		 t
    		 t
    		 :help "Run XeLaTex")
    	       'append))


<a id="orgfa55400"></a>

# Programming

Emacs is most powerful programming environment.  But it is required to be configured in detail, otherwise it will be poor.  Let me configure Emacs for all programming languages I use.


<a id="org54edd1a"></a>

## Generic programming enhancement

Display the number of current line and highlight current line when I am programming.

    (use-package display-line-numbers
      :hook
      (prog-mode-hook . display-line-numbers-mode))
    
    (use-package hl-line
      :hook
      (prog-mode-hook . hl-line-mode))

I have already configured corfu to complete text, activate it when I am programming.

    (use-package corfu
      :ensure t
      :pin gnu
      :hook
      (prog-mode-hook . corfu-mode))


### Back to Indentation or Beginning

Most the real code has the indented structure, and Emacs will move to the beginning of line when press `C-a` by default.  But most times, I mean move to the beginning of code rather than the line.

    (use-package mwim
      :ensure t
      :pin melpa
      :after prog-mode
      :commands
      (mwim-beginning
       mwim-end)
      :init
      (keymap-set prog-mode-map "C-a" 'mwim-beginning)
      (keymap-set prog-mode-map "C-e" 'mwim-end))


### Emacs with Tree-sitter

> Tree-sitter is a parser generator tool and an incremental parsing library. It can build a concrete syntax tree for a source file and efficiently update the syntax tree as the source file is edited. <sup><a id="fnr.3" class="footref" href="#fn.3" role="doc-backlink">3</a></sup>

I use Tree-sitter in Emacs to get faster and fine-grained code highlighting, and more flexible code folding.

Not all programming languages I used support it, so I handle it in the major mode that support it.

    (use-package tree-sitter
      :ensure t
      :pin melpa
      :commands tree-sitter-mode)


### Language Server Protocol

> The Language Server Protocol (LSP) defines the protocol used between an editor or IDE and a language server that provides language features like auto complete, go to definition, find all references etc. The goal of the Language Server Index Format (LSIF, pronounced like "else if") is to support rich code navigation in development tools or a Web UI without needing a local copy of the source code.<sup><a id="fnr.4" class="footref" href="#fn.4" role="doc-backlink">4</a></sup>

To get the LSP support, [lsp-mode.el](https://github.com/emacs-lsp/lsp-mode) and [eglot.el](https://github.com/joaotavora/eglot) both are available for me.  Some people used [lsp-mode.el](https://github.com/emacs-lsp/lsp-mode), but I preferred to use [eglot.el](https://github.com/joaotavora/eglot).  Because [eglot.el](https://github.com/joaotavora/eglot) is considerably less code and hassle than [lsp-mode.el](https://github.com/emacs-lsp/lsp-mode).  In most cases, there's nothing to configure. It's a minimalist approach focused on user experience and performance.

And not all programming languages I used need LSP, so I handle it in the major mode that I really need to use.

    (use-package eglot
      :ensure t
      :pin gnu
      :commands eglot-ensure
      :config
      ;; Auto kill `eglot' server after kill last modifed buffer.
      (setq eglot-autoshutdown t)
    
      ;; Remove `eglot' mode line status information.
      (setq mode-line-misc-info
    	(delete `(eglot--managed-mode
    		  (" [" eglot--mode-line-format "] "))
    		mode-line-misc-info)))


### Find definition and references

Use *xref.el* find definition and references of functions and variables, and get preview feature with [consult.el](https://github.com/minad/consult).

    (use-package xref
      :defines
      (xref-show-xrefs-function
       xref-show-definitions-function))
    
    (use-package consult
      :ensure t
      :pin gnu
      :after xref
      :autoload consult-xref
      :init
      (setq xref-show-xrefs-function #'consult-xref
    	xref-show-definitions-function #'consult-xref))


### On-the-fly syntax checker

Syntax checker checks for syntax errors in each statement, according to the data set type.  And on-the-fly syntax checker show diagnostics while the change affects of code is ongoing.  It help me to write high quality and bug-free code.

I used built-in *flymake.el* rather than [flycheck.el](https://github.com/flycheck/flycheck).


### Show diagnostics in minions menu

I use **Minions Mode** to hide minor modes I don't want to see, and exception and counters of diagnostics will be hide by default.  Exclude it to display in Minions Menu directly.

    (use-package minions
      :ensure t
      :pin melpa
      :defines minions-prominent-modes)
    
    (use-package flymake
      :defer t
      :after minions
      :config
      (when minions-mode
        (push 'flymake-mode minions-prominent-modes)))


### Show diagnostics with preview

Use `completing-read` to jumping between diagnostics, and get preview features from [consult.el](https://github.com/minad/consult).

    (use-package consult
      :ensure t
      :pin gnu
      :after flymake
      :commands consult-flymake
      :init
      (keymap-set flymake-mode-map "C-c !" 'consult-flymake))


<a id="org429744a"></a>

## C/C++

    (use-package tree-sitter
      :ensure t
      :pin melpa
      :hook
      ((c-mode-hook . tree-sitter-mode)
       (c++-mode-hook . tree-sitter-mode)))
    
    (use-package eglot
      :ensure t
      :pin gnu
      :hook
      ((c-mode-hook . eglot-ensure)
       (c++-mode-hook . eglot-ensure)))


<a id="org52c8c5d"></a>

## Emacs Lisp

I use [inspector.el](https://github.com/mmontone/emacs-inspector) to inspect the Emacs Lisp objects.

    (use-package inspector
      :ensure t
      :pin gnu
      :after elisp-mode
      :commands
      (inspector-inspect-last-sexp
       inspector-inspect-expression)
      :init
      (keymap-set emacs-lisp-mode-map
    	      "C-c C-i"
    	      'inspector-inspect-last-sexp)
      (keymap-set emacs-lisp-mode-map
    	      "C-c M-i"
    	      'inspector-inspect-expression))

Emacs Lisp Macros enable I to define new control constructs and other language features.  I use it everyday, *pp.el* is helpful to display pretty expansion of macros.

    (use-package pp
      :after elisp-mode
      :commands
      (pp-macroexpand-last-sexp
       pp-macroexpand-expression)
      :init
      (keymap-set emacs-lisp-mode-map
    	      "C-c C-<return>"
    	      'pp-macroexpand-last-sexp)
      (keymap-set emacs-lisp-mode-map
    	      "C-c M-<return>"
    	      'pp-macroexpand-expression))


<a id="orgd1dc408"></a>

## Haskell

    (use-package haskell-mode
      :ensure t
      :pin nongnu
      :commands haskell-mode)
    
    (use-package tree-sitter
      :ensure t
      :pin melpa
      :hook
      (haskell-mode-hook . tree-sitter-mode))
    
    (use-package eglot
      :ensure t
      :pin gnu
      :hook
      (haskell-mode-hook . eglot-ensure))


<a id="orged938bb"></a>

## Rust

    (use-package rust-mode
      :ensure t
      :pin nongnu
      :commands rust-mode)
    
    (use-package tree-sitter
      :ensure t
      :pin melpa
      :hook
      (rust-mode-hook . tree-sitter-mode))
    
    (use-package eglot
      :ensure t
      :pin gnu
      :hook
      (rust-mode-hook . eglot-ensure))


# Footnotes

<sup><a id="fn.1" href="#fnr.1">1</a></sup> <https://github.com/minad/marginalia#marginaliael---marginalia-in-the-minibuffer>

<sup><a id="fn.2" href="#fnr.2">2</a></sup> <https://en.wikipedia.org/wiki/Drag_and_drop>

<sup><a id="fn.3" href="#fnr.3">3</a></sup> <https://tree-sitter.github.io/tree-sitter/>

<sup><a id="fn.4" href="#fnr.4">4</a></sup> <https://microsoft.github.io/language-server-protocol/>
