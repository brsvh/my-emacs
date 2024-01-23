# GNU Emacs Init File



## Conventions

I maintain the stylistic consistency of the configuration file as it
grows, adhering to conventions for functions, variables, key bindings,
, file storage and ELPA management.


### Naming conventions

I adhere to a straightforward naming convention for functions and
variables.

-   Public functions and variables start with the prefix `my-`.
-   Private or internal functions and variables start with the prefix
    `my--`.
-   Interactive commands start with the prefix `my/`.


### File conventions

The default pathways used for the storage of configuration files and
enduring data exhibit inconsistency across Emacs packages.  This issue
is not confined to third-party packages, but also extends to built-in
ones.

Some packages deposit these files directly in the `user-emacs-directory`
or `HOME`, or in a subdirectory of either, or even elsewhere.
Additionally, file names that fail to provide clarity regarding the
package responsible for their creation are occasionally employed.

Consequently, I undertake the management of these files independently,
adhering to the XDG Base Directory specification to the greatest extent
feasible.

I have classified the Emacs-related files into four distinct categories.

-   Cache: Produced by Emacs Packages, relative to which user-specific
    non-essential data files should be stored.  They specify the location
    of storage via `my-cache-directory`.
-   Config: Crafted by me, their content is of significance to me.  They
    specify the location of storage via `my-config-directory`.
-   Data: Produced by Emacs Packages, relative to which user-specific data
    files should be stored.  They specify the location of storage via
    `my-data-directory`.
-   State: Produced by Emacs Packages, persist between sessions restarts,
    but that is not important or portable enough to the user that it
    should be stored as data file.  They specify the location of storage
    via `my-state-directory`.

In reality, these variables are defined within `early-init`<sup><a id="fnr.1" class="footref" href="#fn.1" role="doc-backlink">1</a></sup>.


### ELPA management conventions

I used `twist`<sup><a id="fnr.2" class="footref" href="#fn.2" role="doc-backlink">2</a></sup> for the management of my used Emacs Lisp packages.
It provide a pure and reproducible method to package my configuration
and all dependent packages into a singular Nix package.  Without
incurring substantial additional costs, I essentially do not require the
composition of any Emacs Lisp code.  My Emacs configuration appears as
though it is employing the built-in `package`.

Furthermore, I employ the `use-package` macro to manage package
configurations in an isolated manner, it provided by the package
`use-package` of the same name.

When configuration alterations are necessitated, by executing an app
named `lock`, all package inputs specified in the configuration via the
*:ensure* keyword are added to the `elpa` directory.  This is achieved
through the parsing of the configuration by the `use-package` parser
provided by `twist`.

    nix run .#lock --impure

Similarly, by executing an app named `update`, all inputs in the
`elpa` directory are updated.

    nix run .#update --impure


#### Simple hacking on `use-package`

As I use the parser provided by `twist` to find the packages that need
to be ensured, some keyword handlers may do some superfluous operations.
Adhering to the principle of cleanliness, I make simple modifications to
them.

    (use-package use-package
      :preface
      (defun my--use-package-fake-ensure-package (&rest _)
        "Do nothing when the need for ensures package installation."
        t)
    
      (defun my--use-package-fake-pin-package (&rest _)
        "Do nothing when pin package to archive."
        t)
      :init
      (setq use-package-hook-name-suffix nil)
    
      (setq use-package-ensure-function
            'my--use-package-fake-ensure-package)
      :config
      (advice-add 'use-package-pin-package
                  :override
                  'my--use-package-fake-pin-package))

As you see in the code above, the *:ensure* keyword of `use-package`
conventionally employs `package` to verify the installation status of
the package.  In reality, I am currently utilizing `twist` to manage all
ELPA, which results in the execution of the superfluous
`package-refresh-contents` during the startup process, circumventing
this occurrence.  For the same reason, it also overrides the handler of
*:pin* keyword.

When employing the *:hook* keyword to delegate a task to the hook,
`use-package` will use an abbreviated hook name.  To illustrate,
`c-mode-hook` is truncated as `c-mode`, and `emacs-startup-hook` is
truncated as `emacs-startup`.  Given my occasional propensity for
oversight, which may lead to inconsistencies, it is imperative to ensure
that `use-package` invariably employs the accurate hook name.


#### My Custom Keyword *:keymap-set*

`use-package` defaults to using keywords such as *:bind*,
*:bind-keymap*, etc., to bind keys.  They are convenient but have an
annoying issue - extremely unsightly indentation, as shown below.

    (use-package term
      :bind (("C-c t" . term)
             :map term-mode-map
             ("M-p" . term-send-up)         ; good indent
             ("M-n" . term-send-down)))
    
    (use-package term
      :bind (:map term-mode-map
                  ("M-p" . term-send-up)    ; indentation broken
                  ("M-n" . term-send-down)))

Consequently, I have crafted my own key binding keywords, *:keymap-set*,
which encapsulate the newer `keymap-set` function.

    (use-package use-package
      :preface
      (defun my--use-package-normalize-keymap-set
          (name keyword args &optional map)
        "`use-package' :keymap-set normalizer."
        (or map (setq map 'global-map))
        (let ((args* args) (ctx-map map) result)
          (while args*
            (cond
             ((and (consp (car args*))
                     (or (stringp (caar args*))
                         (vectorp (caar args*)))
                     (or (use-package-recognize-function (cdar args*)
                                                         t
                                                         #'stringp)))
              (setq result (nconc result (list (list map
                                                     (caar args*)
                                                     `',(cdar args*))))
                    args* (cdr args*)))
             ((keywordp (car args*))
              (setq ctx-map (intern (substring (symbol-name (car args*))
                                               1))
                    result (nconc result
                                  (my--use-package-normalize-keymap-set
                                   name keyword (cdr args*) ctx-map))
                    args* nil))
             ((listp (car args*))
              (setq result (nconc result
                                  (my--use-package-normalize-keymap-set
                                   name keyword (car args*) ctx-map))
                    args* (cdr args*)))
             (t
              (use-package-error
               (concat (symbol-name name)
                       " wants arguments acceptable to the `keymap-set'"
                       " function, or a list of such values")))))
          result))
    
      (defun my--use-package-handle-keymap-set
          (name keyword args rest state)
        "`use-package' handler of :keymap-set keyword."
        (use-package-concat
         (mapcar
          #'(lambda (keybinding) `(keymap-set ,@keybinding))
          args)
         (use-package-process-keywords name rest state)))
    
      (defun my--use-package-set-keyword (keyword position)
        "Insert KEYWORD to `use-package-keywords' after POSITION."
        (unless (member keyword use-package-keywords)
          (unless (and (keywordp keyword)
                       (keywordp position))
            (error "KEYWORD and POSITION must be keyword"))
          (setq use-package-keywords
                (let* ((pos (cl-position position use-package-keywords))
                       (head (cl-subseq use-package-keywords 0 (+ 1 pos)))
                       (tail (nthcdr (+ 1 pos) use-package-keywords)))
                  (append head (list keyword) tail)))))
    
      :init
      (my--use-package-set-keyword :keymap-set :custom-face)
    
      (defalias 'use-package-normalize/:keymap-set
        'my--use-package-normalize-keymap-set)
    
      (defalias 'use-package-handler/:keymap-set
        'my--use-package-handle-keymap-set)
    
      (defalias 'use-package-autoloads/:keymap-set
        'use-package-autoloads-mode))

In this manner, a keymap can be specified for binding through a keyword
named with the actual symbol name of keymap.

    (use-package foo
      :commands (foo bar)
      :keymap-set
      ("C-c f" . foo)      ;; => (keymap-set global-map "C-c f" 'foo)
      (:foo-mode-map
       ("C-c f" . foo)     ;; => (keymap-set foo-mode-map "C-c f" 'foo)
       ("C-c b" . bar)     ;; => (keymap-set foo-mode-map "C-c b" 'bar)
       :bar-mode-map
       (("C-c f" . foo)    ;; => (keymap-set bar-mode-map "C-c f" 'foo)
        ("C-c b" . bar)))) ;; => (keymap-set bar-mode-map "C-c b" 'bar)


### Keybinding conventions

For all commands, both those I have created and those from other
packages, I bind them to <kbd> C-c </kbd> for invocation via a sequence of
key combinations.

I have defined a keymap, denoted as `ctl-c-map`, analogous to
`ctl-x-map`, binding all commands with <kbd> C-c </kbd> as a prefix key to
it.

    (defvar ctl-c-map (make-keymap)
      "Default keymap for C-c commands.")
    
    (keymap-set global-map "C-c" ctl-c-map)

Typically, the major mode command is bound to a key under the prefix key
<kbd> C-c </kbd> and necessitates holding down <kbd> Control </kbd>.  For
instance, <kbd> C-c C-e </kbd> is bound to `elisp-eval-buffer` in
`emacs-lisp-mode`, <kbd> C-c C-l </kbd> is bound to `org-insert-link` in
`org-mode`, and so forth.

Consequently, I bind the commands I require to the letter keys with
<kbd> C-c </kbd> as the prefix key. These letter keys are also employed
for categorization purposes.

-   <kbd> m </kbd>: Major mode commands.
-   <kbd> n </kbd>: Nix(Flake) commands.
-   <kbd> v </kbd>: Version Control commands.

    (defvar ctl-c-m-map (make-keymap)
      "Default keymap for C-c m commands.")
    
    (defvar ctl-c-n-map (make-keymap)
      "Default keymap for C-c n commands.")
    
    (defvar ctl-c-v-map (make-keymap)
      "Default keymap for C-c v commands.")
    
    (keymap-set ctl-c-map "m" ctl-c-m-map)
    (keymap-set ctl-c-map "n" ctl-c-n-map)
    (keymap-set ctl-c-map "v" ctl-c-v-map)


### Init Conventions

The [startup](elisp#Startup Summary) of Emacs is rather
intricate, and I harbor certain compulsions towards controlling this
process.  I anticipate that the most of Minor Modes should use hooks to
regulate their activation and deactivation, hence I have some
conventions for managing the startup process.

All settings related to the User Interface during the initialization
process will be activated via `emacs-startup-hook`, otherwise, they will
be activated through `after-init-hook`.

    (use-package startup
      :no-require t
      :defines
      (after-init-hook emacs-startup-hook))


## Performance

As an Emacs user dealing with demanding tasks, Emacs is
performance-sensitive.  Given my lack of patience, I consistently harbor
enthusiasm for Emacs' performance optimization, acceleration, and
diagnostics.


### Intelligent garbage collection strategy

The majority of Emacs Lisp Packages are written in Emacs Lisp,
supporting automatic memory management.  By default, Emacs used a fixed
threshold to initiate Garbage Collection, a method that lacks
flexibility and may lead to performance deterioration due to frequent
Garbage Collection.  To minimize the interference of Garbage Collection
with user activity, I adopt a cunning Garbage Collection strategy
provided by gcmh<sup><a id="fnr.3" class="footref" href="#fn.3" role="doc-backlink">3</a></sup>.  This strategy utilizes a low Garbage Collection
threshold when idle, and a high threshold during regular operation.

    (use-package gcmh
      :ensure gcmh
      :hook
      (emacs-startup-hook . gcmh-mode))


### Using Emacs as a server

As Emacs users continually incorporate new Emacs Lisp Packages, a
convenient approach is to operate Emacs in the classic Server-Client
mode, advancing the startup time overhead, thereby making the delay
imperceptible.

Furthermore, some conveniences can be obtained.  When I invoke Emacs in
other locations, I desire to share buffers, a command history, or
supplementary data with the existing Emacs process.  Using the Emacs
Client can achieve this.

    (use-package server
      :preface
      (defun my/server-start ()
        "Allow this Emacs process to be a server for client processes."
        (interactive)
        (eval-when-compile (require 'server))
        (unless (server-running-p) (server-start)))
    
      :config
      (setq server-auth-dir
            (expand-file-name "server/" my-state-directory))
    
      :hook
      (emacs-startup-hook . my/server-start))


## Customization system

Emacs is a real-time, extensible, and customizable editor.  Its
customization system, **Easy Customize**, allows users to modify existing
commands or add new ones. Users can navigate through a list of settings,
edit and set their values, and save them permanently.


### Easy Customization Interface

Despite my abstention from utilizing the Easy Customization Interface of
Emacs, certain packages surreptitiously employ it unbeknownst to me.  In
pursuit of maintaining the integrity of the `user-init-file` and my
configuration file, devoid of auto-generated content, I opt to preserve
customizations in an alternative location, distinct from my
initialization file.

    (setq custom-file (expand-file-name "custom.el" my-state-directory))
    
    (when (file-exists-p custom-file)
      (load custom-file nil 'nomessage))


## User interface

The inherent interface of Emacs exudes a vintage and traditional aura,
which, as a youthful individual, I find less appealing.  My preference
leans towards a more contemporary aesthetic.


### Default frame layout

Emacs facilitates operations on both graphical and non-graphical frames.
The default frame layout exhibits a menu bar, tool bar, and vertical
scroll bar.  However, in the character-only frame, the tool bar and
vertical scroll bar are absent.

While the menu bar, tool bar, and scroll bar serve practical purposes,
my operational flow is predominantly keyboard-centric.  Given their
infrequent usage and their propensity to cause distractions, I have
chosen to disable them.

An additional, albeit less significant, rationale is that these bars are
initialized prior to the evaluation of the init file (post
`before-init-hook`), which inadvertently decelerates Emacs startup.
Disabling them consequently enhances Emacs' speed, of course, it needs
to be done in `early-init`<sup><a id="fnr.1.100" class="footref" href="#fn.1" role="doc-backlink">1</a></sup>.

    (use-package emacs
      :no-require t
      :init
      (menu-bar-mode -1)
      (tool-bar-mode -1))
    
    (use-package scroll-bar
      :when (display-graphic-p)
      :init
      (scroll-bar-mode -1))


#### Mode Line

The [Mode Line](emacs#Mode Line), situated towards the bottom of
Emacs, provides details about the current buffer, encompassing aspects
such as encoding, buffer size, cursor position, major mode, and minor
mode.

I employ moody<sup><a id="fnr.4" class="footref" href="#fn.4" role="doc-backlink">4</a></sup> to acquire a mode line styled with tabs and ribbons.
Its merit lies in its minimalistic alterations to the mode line, merely
adding or substituting a select few elements.

    (use-package moody
      :ensure moody
      :hook
      (emacs-startup-hook . moody-replace-eldoc-minibuffer-message-function)
      (emacs-startup-hook . moody-replace-mode-line-buffer-identification)
      (emacs-startup-hook . moody-replace-vc-mode))

By default, Emacs draws underlines at the baseline of the font, and some
elements in the Mode Line, such as **Buffer Name** and **Version Control
Menu**, are underlined.  This results in a conspicuous and unsightly
underline displayed in the Mode Line.  I have modified
`x-underline-at-descent-line` to `t`, instructing Emacs to draw
underlines at the same height as the font descent line.

    (use-package emacs
      :no-require t
      :init
      (setq-default x-underline-at-descent-line t))


#### Replace Minor Mode menu with a button

Upon activation of numerous minor modes in a buffer, the Mode Line
consequently displays an extensive list of minor mode names.  This is
less than ideal, as overly verbose mode information can result in other
useful details falling outside the frame's scope.  To address this, I
employ `minions`<sup><a id="fnr.5" class="footref" href="#fn.5" role="doc-backlink">5</a></sup> to supplant the minor modes list with a menu button
that enumerates the enabled minor modes.

    (use-package minions
      :ensure t
      :defines
      (minions-prominent-modes)
      :init
      (setq minions-mode-line-lighter ":)"
            minions-mode-line-delimiters '("" . ""))
      :hook
      (emacs-startup-hook . minions-mode))


### Default startup screen

Emacs exhibits a **\\\*GNU Emacs\\\*** buffer, which I find to be of limited
utility.  As an alternative, I employ the **\\\*scratch\\\*** buffer as the
default screen, ensuring to meticulously cleanse its content and the
echo area.

    (use-package startup
      :no-require t
      :init
      (setq inhibit-startup-screen t
            inhibit-startup-echo-area-message t
            initial-scratch-message nil
            initial-major-mode 'fundamental-mode))


### Accessible theme

Emacs' inherent theme is somewhat lackluster, lacking the polished and
lustrous appearance that typically appeals to novices, often resulting
in a superficial and unfavorable initial impression of the
system. Subsequently, I use Modus Themes<sup><a id="fnr.6" class="footref" href="#fn.6" role="doc-backlink">6</a></sup> that enhances Emacs'
aesthetic appeal, rendering it more user-friendly.

    (use-package modus-themes
      :ensure modus-themes
      :preface
      (defun my-theme-init ()
        "Initialize theme."
        (if (display-graphic-p)
            (load-theme 'modus-operandi-tinted :no-confirm)
          (load-theme 'modus-vivendi-tinted :no-confirm)))
      :config
      (setq modus-themes-custom-auto-reload t
            modus-themes-bold-constructs t
            modus-themes-italic-constructs t
            modus-themes-mixed-fonts t
            modus-themes-prompts '(bold)
            modus-themes-completions '((matches . (extrabold))
                                       (selection . (semibold
                                                     fitalic
                                                     text-also)))
            modus-themes-org-blocks 'tinted-background
            modus-themes-headings '((1 . (1.30 extrabold))
                                    (2 . (1.20 heavy))
                                    (3 . (1.10 bold))
                                    (t . (1.05 semibold))))
      :hook
      (after-init-hook . my-theme-init))

As you may have noticed, I have used a rudimentary configuration of
Modus Themes.  When utilizing the graphical interface of Emacs, I prefer
a light theme, whereas in other scenarios, I opt for a dark theme.
These other scenarios typically involve interfaces that solely support
characters, such as remote access via SSH or launching Emacs in a
terminal.  Predominantly, I desire a more conspicuous style for my Emacs
interface, hence you observe my activation of extensive italic and bold
support.

By default, Mode Line renders a box effect, which is essentially a
border around the Mode Line. I aspire for a borderless modeline, which
can be achieved through `modus-themes-common-palette-overrides`.

    (use-package modus-themes
      :config
      (push '(border-mode-line-active unspecified)
            modus-themes-common-palette-overrides)
      (push '(border-mode-line-inactive unspecified)
            modus-themes-common-palette-overrides))

Furthermore, there is an additional rectification wherein I have ensured
uniformity between the background color of the current line and the
current line number.  However, this rectification lacks elegance.
Ideally, it should use the overlay
`modus-themes-common-palette-overrides` provided by Modus Themes for
adjustments, but it is currently non-functional and awaits future
rectification.

    (defun my--modus-themes-enale-p ()
      "Return t if current theme is belong to Modus Themes, else nil."
      (cl-some #'(lambda (theme)
                   (member theme '(modus-operandi
                                   modus-operandi-tinted
                                   modus-vivendi
                                   modus-vivendi-tinted)))
               custom-enabled-themes))
    
    (use-package display-line-numbers
      :config
      (defun my--reset-modus-themes-line-number-face ()
        "Use the more subtle line number background color."
        (when (my--modus-themes-enale-p)
          (let* ((cline 'line-number-current-line)
                 (oline 'line-number)
                 (proper-bg (face-attribute oline :background)))
            (set-face-attribute cline nil :background proper-bg))))
      :hook
      (display-line-numbers-mode-hook
       .
       my--reset-modus-themes-line-number-face))
    
    (use-package hl-line
      :config
      (defun my--reset-modus-themes-line-number-face-when-highlight ()
        "Use the more subtle line number background color."
        (when (my--modus-themes-enale-p)
          (let* ((cline 'line-number-current-line)
                 (hline 'hl-line)
                 (oline 'line-number)
                 (origin-bg (face-attribute oline :background))
                 (proper-bg (face-attribute hline :background)))
            (if hl-line-mode
                (setq proper-bg (face-attribute hline :background))
              (setq proper-bg origin-bg))
            (set-face-attribute cline nil :background proper-bg))))
      :hook
      (hl-line-mode-hook
       .
       my--reset-modus-themes-line-number-face-when-highlight))


### Window operation

Emacs refers to the display area of a buffer as a Window. As buffers are
continuously created and discarded, Emacs users incessantly manipulate
these Windows.  However, much of this manipulation leaves room for
improvement, which I intend to address in the subsequent steps.


#### Windows switching

Emacs employs the shortcut <kbd> C-x o </kbd>, also known as
`other-window`, to switch between windows, allowing for sequential
navigation to the subsequent window.  In reality, during my utilization
of Emacs for various tasks, the number of windows opened within a single
Frame frequently exceeds two.  This necessitates repeated pressing of
<kbd> C-x o </kbd> to cycle through the windows, an operation that can be
mentally and physically exhausting.  To alleviate this, I employ
`switch-window`<sup><a id="fnr.7" class="footref" href="#fn.7" role="doc-backlink">7</a></sup> as a remedy.

    (use-package switch-window
      :ensure switch-window
      :keymap-set
      ("<remap> <other-window>" . switch-window))


#### Window splitting

Emacs, through the shortcuts <kbd> C-x 2 </kbd> and <kbd> C-x 3 </kbd>,
facilitates horizontal and vertical window splitting, effectively
creating new windows either below or to the right.  The default
splitting behavior proves efficient for one or two windows.  However,
once this number is exceeded, I am compelled to switch focus between
windows before selecting a new splitting, a process that can be somewhat
cumbersome.  To streamline this, I use the convenient commands provided
by `switch-window` to replace the bindings of `split-window-below`,
`split-window-right`, and `delete-other-windows`, thereby accomplishing
window switching and splitting in one fell swoop.

    (use-package switch-window
      :keymap-set
      ("<remap> <delete-other-windows>" . switch-window-then-maximize)
      ("<remap> <split-window-below>" . switch-window-then-split-below)
      ("<remap> <split-window-right>" . switch-window-then-split-right))


## Multilingual environment

As a polyglot (it may not qualify as such, but I want to be.), it is
only natural for me to use Emacs for editing text in various languages.
Fortuitously, Emacs' multilingual environment extends support to
virtually all coding systems.


### UTF-8

Within the realm of Linux, UTF-8 is the superior encoding methodology
for Unicode.  Consequently, I default to employing UTF-8, barring
instances where the encoding is explicitly delineated otherwise.

    (use-package mule-cmds
      :no-require t
      :init
      (set-default-coding-systems 'utf-8)
      (set-language-environment "utf-8")
      (prefer-coding-system 'utf-8))


### Multibyte characters

In the realm of character encoding, the most of linguistic symbols in
the world are composed of multiple bytes, and the range of byte values
can confirm the language to which the character belongs.  Within Emacs,
`character` offers a predefined description, referred to as **Category**,
thereby enabling additional settings for different languages via
Category.


#### Multibyte character text wrapping

Emacs supports a variety of line wrapping methods, primarily based on
the value of `fill-column` to perform hard or soft wrap.  By default,
its compatibility with multibyte characters is subpar, manifesting as
line wrapping at incorrect positions.  I default to enabling word wrap
based on Category.

    (use-package emacs
      :no-require t
      :init
      (setq-default word-wrap-by-category t))


## File and Text editing

As persistently discussed in this document, I utilize Emacs to
accomplish a myriad of tasks.  The vast majority of these tasks involve
text operation, which is not solely because I use Linux (where
everything is a file in Unix-like operating systems), but also because
the majority of what the Emacs interface presents is character-based.
Therefore, most of my operations in Emacs are essentially modifying
characters, or in other words, editing text.

The efficiency of text editing is a frequently discussed topic among
Emacs users, and I am no exception.  I need to make endless improvements
to my Emacs configuration to achieve a more efficient text editing
experience.  So, what are we truly focusing on when editing text? From
my personal perspective, the main functionalities of interest are
navigation, search, replacement, and disaster recovery.


### More detailed and user-friendly information prompts

While editing files and texts, I desire more user-friendly information
prompts to help me understand the extent of my modifications, such as
whether I have saved them, cursor position, file location, and so on.

Regrettably, despite the provision of related functionality with the
distribution of Emacs, the presets of Emacs do not enable these.

I always expect Emacs to display the row and column positions of the
current cursor, and to be able to display the size of the current
Buffer.  I achieve this by enabling some minor modes provided in
`simple`.

    (use-package simple
      :hook
      (emacs-startup-hook . column-number-mode)
      (emacs-startup-hook . line-number-mode)
      (emacs-startup-hook . size-indication-mode))

When the text I am modifying has corresponding files, the Buffer name
will use their file paths and display the basename.  However, when I
open two files that have the same basename, the buffers of these files
will append the parent directory name with brackets.  This appears
different from the Unix/Linux file path, and I prefer using prefix paths
to distinguish them.

    (use-package uniquify
      :config
      (setq uniquify-buffer-name-style 'forward))


### Text replacement

Text replacement is one of the common scenarios in file editing.
Evidently, Emacs supports this, but the problem is that the presets are
not user-friendly.

When I am operating on a segment of multi-character text, I first select
them, then delete or replace them with new text.  Strangely, Emacs does
not replace the selected text with new input by default, but instead
appends it.  This is counter-intuitive, and I enable
`delete-selection-mode` to make it more natural.

    (use-package delsel
      :hook
      (emacs-startup-hook . delete-selection-mode))


### Text cutting, copying and pasting

Emacs maintains its own clipboard, but strictly speaking, it cannot be
called a clipboard, but rather a Kill Ring.  The kill ring is a
compilation of text blocks that were previously eliminated.  The
clipboard is the mechanism that most graphical applications utilize for
cutting, copying and pasting.  Therefore, I believe that, informally,
the kill ring can be referred to as a clipboard.

I want to synchronize the content of kill ring and the system's
clipboard.  Consequently, I need to configure Emacs to save the kill
ring to the clipboard upon exit.

    (use-package simple
      :config
      (setq kill-do-not-save-duplicates t
            save-interprogram-paste-before-kill t))

Also use `kill-do-not-save-duplicates` de-duplication to sift out
duplicates.

For excessively lengthy content, I opt to obtain further preview support
through `consult`, which assists me in glimpsing the result after
modifications have taken effect in the target modification area.

    (use-package consult
      :keymap-set
      ("<remap> <yank>" . consult-yank-from-kill-ring)
      ("<remap> <yank-pop>" . consult-yank-pop))


### Disaster recovery

In this context, disaster recovery refers to how I recover modifications
made to a file after the buffer has been accidentally closed or Emacs
has unexpectedly exited.  In Emacs, we can maintain composure in the
face of unexpected file editing incidents through two methods: auto-save
and backup.


#### Automatic saving

Automatic saving, like the majority of editors worldwide, periodically
backs up the file being edited, or more aptly put, creates a snapshot.

Inherently, Emacs periodically auto-saves your modifications to a file.
In the event of an unforeseen circumstance, you can retrieve a file
using <kbd> M-x recover-file RET </kbd>.

Emacs generates the auto-saved file by appending a # to both ends of the
visited file name in place.  To maintain a tidy directory and adhere to
my [File conventions](#org114ad6d), I apply my custom transformation rule for
creating auto-save file names to `auto-save-file-name-transforms`.

    (use-package files
      :config
      (setq auto-save-default t
            auto-save-no-message t
            auto-save-include-big-deletions t
            auto-save-file-name-transforms
            (append `((".*"
                       ,(expand-file-name "auto-save/" my-data-directory)
                       t))
                    auto-save-file-name-transforms)))

By default, erasing a significant portion of the text deactivates
auto-save in the buffer.  Consequently, if I make numerous alterations
and my computer abruptly malfunctions, all my changes will be forfeited.
Therefore, it is imperative to save all changes regardless. To ensure
failsafe always takes precedence, I must set
`auto-save-include-big-deletions` to t.


#### Automatic backup

A backup refers to a copy of the original file prior to revision.  At
face value, it may seem as though I have already obtained a historical
backup of the file through auto-saving.  However, in reality, the backup
referred to here is vastly different from auto-saving.  This is because a
single revision of a file may encompass multiple auto-saves.  After all,
I am not a ceaseless typewriter, so the majority of auto-save archives
are unsuitable as versions before and after revision.

By default, Emacs saves backup files—those ending in `~` —in the current
directory, thereby leading to clutter.  Let's relocate them to a
directory in accordance with my [File conventions](#org114ad6d).

I aim to retain multiple versions of my backup files to help preserve my
sanity.  Emacs permits the saving of an unlimited number of backups, but
maintaining a backup of five versions seems appropriate.

    (use-package files
      :config
      (setq make-backup-files t
            version-control t)
    
      (setq backup-by-copying t)
    
      (setq delete-old-versions t
            kept-old-versions 5
            kept-new-versions 5)
    
      (setq backup-directory-alist
            (append `(("."
                       .
                       ,(expand-file-name "backup/" my-data-directory)))
                    backup-directory-alist)))

You may have observed that I set `backup-by-copying` to `t`, which
prompts Emacs to create backups by duplicating the original file.  This
deviates from the default method, which generates backups by renaming
the original file.


## Completion

The completion feature in Emacs significantly enhances my interaction
with the Emacs.  Primarily, Emacs offers two types of completion:

-   Input completion, which provides completion when entering input in the
    minibuffer.
-   Text completion, which provides completion during text editing in the
    buffer.

Given the substantial differences in the scenarios for these two types
of completion, I refrain from employing a uniform completion interaction
method for both.


### Input completion

My choice is to provided input completion support with `vertico` <sup><a id="fnr.8" class="footref" href="#fn.8" role="doc-backlink">8</a></sup>,
which is a performant, minimalistic vertical completion UI for Emacs.
It reuses built-in facilities for full compatibility with Emacs'
completion commands and tables.

It can be effortlessly enabled by activating **Vertico Mode**.  Of course,
additional configurations are indispensable for catering to personalized
requirements.

Upon the initiation of **Vertico Mode**, a minibuffer of immutable
dimensions is exhibited during the completion process.  My inclination,
however, is towards a flexible height contingent upon the quantity of
remaining candidates.

An additional facet necessitates refinement.  Upon navigating to the
last candidate, I require assistance to expediently back to the top.
Consequently, I enable the cyclical functionality of `vertico-next` and
`vertico-previous`.

By default, `find-file` initiates the opening of files residing in the
current directory, rendering the modification of the `find-file` path a
prevalent procedure.  Regrettably, the default setting permits the
deletion of a solitary character at a time, a process that is not only
cumbersome but also exhausts my patience.  Fortunately, `vertico`
incorporates an extension, `vertico-directory`, which proffers commands
capable of eliminating multiple characters in the path simultaneously by
word.  I have elected to assign these commands to the
<kbd> <backspace> </kbd> key.

And a litte help, I set **Vertico Mouse Mode** to use the mouse to select
the candidates.

    (use-package vertico
      :ensure vertico
      :pin gnu
      :config
      (setq vertico-resize t)
    
      (setq vertico-cycle t)
      :hook
      (emacs-startup-hook . vertico-mode))
    
    (use-package vertico-directory
      :ensure vertico
      :pin gnu
      :after vertico
      :keymap-set
      (:vertico-map
       ("<return>" . vertico-directory-enter)
       ("<backspace>" . vertico-directory-delete-char)
       ("M-<backspace>" . vertico-directory-delete-word)))
    
    (use-package vertico-mouse
      :ensure vertico
      :pin gnu
      :after vertico
      :hook
      (vertico-mode-hook . vertico-mouse-mode))


#### Completion style

Emacs accommodates an array of completion styles, with three variants
activated by default: `basic`, `emacs22`, and `partial-completion`.

These correspond to:

-   `basic`: Completion of the prefix preceding the cursor and the suffix
    following the cursor.
-   `emacs22`: Prefix completion that exclusively operates on the text
    preceding the cursor.
-   `partial-completion`: Completion of multiple words, each treated as a
    prefix.

For instance, given a command `foo-bar`, it could be completed with
these inputs:

-   f\_b, completed by the `basic` style
-   f\_r, completed by the `emacs22` style
-   f-\_a, completed by the `partial-completion` style.

Here, \_ denotes the cursor's position.

While the default is adequate, I prefer to be permitted to complete the
input using spaces as separators.  Consequently, I use orderless<sup><a id="fnr.9" class="footref" href="#fn.9" role="doc-backlink">9</a></sup>,
which offers a completion style named orderless that segments the
pattern into space-separated components and matches them.

Furthermore, enable `partial-completion` for file path expansion.
`partial-completion` is crucial for file wildcard support.  Multiple
files can be simultaneously opened with `find-file` if the input
contains a wildcard.

    (use-package minibuffer
      :ensure orderless
      :init
      (setq completion-styles '(substring orderless basic)
            completion-category-overrides
            '((file (styles basic partial-completion)))))


#### Show mariginalia of completions

Marginalia are marks or annotations placed at the margin of the page of
a book or in this case helpful colorful annotations placed at the margin
of the minibuffer for your completion candidates.<sup><a id="fnr.10" class="footref" href="#fn.10" role="doc-backlink">10</a></sup>

I use it to glance at docstring, the values of variables, and even file
permissions.

    (use-package marginalia
      :ensure marginalia
      :pin gnu
      :config
      (setq marginalia-align 'right)
      :hook
      (emacs-startup-hook . marginalia-mode))


## Project management

When I am engaged in programming or writing, the majority of my trivial
tasks, operations, and text editing are project-based, that is, a
collection of files used for the production of one or more products.
Consequently, I aspire for Emacs to accomplish the majority of
interactive operations required during my work within projects, such as
file positioning, symbol searching, version management, task planning,
and so forth.


### Version control

Version control is a system that records changes to a file or set of
files over time so that you can recall specific versions later, this is
particularly beneficial for maintaining records and facilitating
collaborative work.  In the majority of my tasks, I incessantly need to
check out remote repositories, manipulate local version data, and push
to remote repositories.  Fortunately, Emacs significantly alleviates the
stress associated with these activities.


#### Git

Git is currently the most popular distributed version control system in
the world, and naturally, I cannot afford to be the exception in not
using it.  Emacs, on the other hand, is the optimal client for Git,
specifically, Emacs equipped with Magit<sup><a id="fnr.11" class="footref" href="#fn.11" role="doc-backlink">11</a></sup>.  I am acquainted with
numerous users who have newly joined the Emacs community, their
migration from other editors to Emacs is primarily motivated by the
desire to use magit.  Of course, I too wish to use the best resources,
from Emacs to magit.

Magit is ready to use out of the box, yet I have still made some
modifications.  I have unbound its default annoying key bindings,
<kbd> C-x g </kbd> and <kbd> C-c g </kbd>, and we have bound all the magit
commands required to the <kbd> C-c v g </kbd> prefix, which signifies the
Git subcategory of the Version control category.

    (defvar ctl-c-v-g-map (make-keymap)
      "Default keymap for C-c v g commands.")
    
    (keymap-set ctl-c-v-map "g" ctl-c-v-g-map)
    
    (use-package magit
      :ensure magit
      :init
      (setq-default magit-define-global-key-bindings nil)
      :keymap-set
      (:ctl-c-v-g-map
       ("d" . magit-dispatch)
       ("s" . magit-status)))


### Nix

Nix is the tool I use to manage dependencies in the most of my
programming projects, specifically, which use Nix Flake.  I am currently
experimenting with Nix3.el<sup><a id="fnr.12" class="footref" href="#fn.12" role="doc-backlink">12</a></sup>, an Emacs Nix Interface akin to Magit,
which I employ to manage the inputs and outputs of Nix Flake projects,
as well as interactive operations that I prefer not to execute outside
of Emacs.

I have bound most of the Nix3.el commands to the <kbd> C-c n </kbd> prefix
keymap, where <kbd> n </kbd> subtly signifies Nix.

    (use-package nix3
      :ensure nix3
      :keymap-set
      (:ctl-c-n-map
       ("b" . nix3-build)
       ("d" . nix3-transient)
       ("e" . nix3-flake-edit)
       ("i" . nix3-flake-init)
       ("n" . nix3-flake-new)
       ("r" . nix3-run)
       ("s" . nix3-flake-show)))

Furthermore, the section that appends inputs and outputs information to
the **Magit Status Buffer** by enabling `magit-nix3-flake-mode`.

    (use-package magit-nix3
      :disabled
      :ensure magit-nix3
      :after magit-status
      :hook
      (magit-status-mode-hook . magit-nix3-flake-mode))


## Writing

Emacs is an indispensable companion for authors, the abundant extensions
from the Emacs community and the extensibility of Emacs itself proffer
boundless opportunities for writing.

However, the most of these are not ready to use out of the box, or
rather, they are not enabled by default.  At the same time, I do not
wish to enable them as global settings.  Therefore, you will see that
most of my writing configurations are tailored for those text editing
modes derived from **Text Mode** and **Outline Mode**.

During my writing, I desire Emacs to highlight my current position
within the buffer, which aids in visually pinpointing the editing
location.  Contrary to programming, I am indifferent to the absolute
position of the editing location during writing, referring here to the
file path and row-column coordinates; I merely require the current
editing line to be highlighted.

    (use-package hl-line
      :commands
      (hl-line-mode))
    
    (use-package outline
      :hook
      (outline-mode-hook . hl-line-mode))
    
    (use-package text-mode
      :hook
      (text-mode-hook . hl-line-mode))

Emacs supports a variety of line-folding methods<sup><a id="fnr.13" class="footref" href="#fn.13" role="doc-backlink">13</a></sup>, including:

-   **Hard Wrap** ：Modes such as AutoFillMode insert a line ending after
    the last word that occurs before the value of option `fill-column` (a
    column number).
-   **Soft Wrap** ：Modes such as VisualFillColumn (in concert with
    VisualLineMode) wrap a line after the last word before `fill-column`,
    but ultimately they do not alter the buffer text.  Such **soft**
    wrapping is essentially a display effect.
-   **Default Wrap** : Emacs wraps a line that reaches the window width,
    except at a word boundary.  The buffer text is not changed.

In the most of my cases, I employ the **Soft Wrap** method.

    (use-package simple
      :commands
      (visual-line-mode))
    
    (use-package outline
      :hook
      (outline-mode-hook . visual-line-mode))
    
    (use-package text-mode
      :hook
      (text-mode-hook . visual-line-mode))


### Writing with `org-mode`

**Org Mode** is a major mode within GNU Emacs for editing the org file
format, and the org file format is a markup text language replete with
rich features.  The majority of my plain text files are composed in the
org file format.  I can leverage Emacs or other external tools (such as
`pandoc`) to flexibly convert between org files and other file formats.
Additionally, I use `org-mode` to attain superior computable tables
(compared to Markdown), literate programming, and inline support for
LaTeX.

When I am use Org Mode for writing, superficially, my primary concern is
aesthetics, hence I have racked my brains to strike a balance between
pure text editing and modern appearance within Org Mode.  The majority
of the time, I am concerned with the original content of paragraph text,
thus I anticipate that the markup syntax used in Org Mode paragraphs
retains the original characters, rather than being overlaid with other
symbols or omitted.  This typically refers to hiding emphasis markers,
descriptive links, etc.  However, beyond the content of the paragraphs,
I desire some replacements for aesthetic design considerations, such as
the heading star can use more visually pleasing symbols, tags or
keywords can use special icons, etc.  Therefore, I use
`org-modern`<sup><a id="fnr.14" class="footref" href="#fn.14" role="doc-backlink">14</a></sup> to enhance the appearance of Org Mode headings by
replacing the asterisk symbols with more appealing circles, and it also
enhances the looks of plain lists, todo items, and tables.

    (use-package org
      :ensure org
      :defer t
      :config
      (setq org-auto-align-tags nil
            org-hide-emphasis-markers nil
            org-pretty-entities t
            org-special-ctrl-a/e t
            org-startup-with-inline-images t
            org-tags-column 0))
    
    (use-package org-modern
      :ensure org-modern
      :after org
      :hook
      (org-mode-hook . org-modern-mode))

You may have noticed that I have activated an special option
`org-special-ctrl-a/e`.  When I am editing the headline, I adjust the
headline level with the tab key.  Therefore, <kbd> C-a </kbd> returning to
before the leading star is not suitable, as I invariably anticipate
modifying the first word of the headline rather than altering the level.
Set `org-special-ctrl-a/e` to shift the cursor to the beginning of the
headline when <kbd> C-a </kbd> is pressed.  The same applies to
<kbd> C-e </kbd>, which moves to the end of the headline when editing a
folded block.

Upon completing the aforementioned configurations, I achieved a visually
appealing aesthetic enhancement for Org Mode.  However, its default
hierarchical heading symbols remain somewhat unsightly.  In certain
fonts, the size of the diamond symbol is inconsistent with that of the
circle symbol.  I used a more uniform set of circle symbols to
distinguish between heading levels.

    (use-package org-modern
      :config
      (setq org-modern-star '( "●" "◉" "◎" "○" "◌")))


#### Displaying Outline on the Sidebar

When I am composing or perusing an exceedingly lengthy org, I anticipate
the presence of a sidebar, listing all the headings within the Org file,
essentially an outline preview.  This is one of the most coveted
features for those using Org Mode for writing, as it facilitates
effortless navigation between chapters and scenes in novels or other
extensive works.  I use `org-sidebar-tree`<sup><a id="fnr.15" class="footref" href="#fn.15" role="doc-backlink">15</a></sup> to achieve this.

    (use-package org-side-tree
      :ensure org-side-tree
      :after org
      :commands
      (org-side-tree)
      :keymap-set
      (:org-mode-map
       ("C-c m o" . org-side-tree))
      :hook
      (org-side-tree-mode-hook . org-indent-mode))


#### File Export Support

The default distribution of Org Mode already supports exporting to
renowned file formats in the open world, such as Markdown, LaTeX, ODT,
Texinfo, etc.  However, Org Mode only enables a small portion of its
backends by default—HTML, LaTeX, Plain Text—which is far from sufficient
for my daily use.  Therefore, I need to add more default backends.

I except that during the execution of export operations, the specifics
can be controlled via Local Variables, hence I invariably permit the
usage of the BIND keyword.

    (use-package org
      :defer t
      :config
      (setq org-export-allow-bind-keywords t))

-   Markdown

    When exporting an Org file to Markdown, the default behavior appears
    somewhat peculiar, or rather, it does not align with my requirements.  I
    anticipate that the Title will export to a Level 1 headline in Markdown,
    while the headlines in the Org file will be demoted one level
    accordingly.  However, Emacs does not export the Title by default, and
    the first level in Org corresponds to the first level in Markdown.  I
    will make some adjustments to meet my expectations.
    
        (use-package ox-md
          :after org
          :config
          (defvar-local org-md-export-title-as-hlevel-1 t
            "Non nil export TITLE as the level 1 heading line.")
        
          (defun org-md-export-title-to-hlevel-1 (orig-fun &rest args)
            "An advice for export TITLE as the level 1 heading line."
            (let* ((res (apply orig-fun args))
                   (title (org-element-interpret-data (plist-get (nth 1 args)
                                                                 :title))))
              (if org-md-export-title-as-hlevel-1
                  (concat "# " title "\n\n" res)
                res)))
        
          (advice-add 'org-md-template
                      :around
                      #'org-md-export-title-to-hlevel-1)
        
          (setq-default org-md-toplevel-hlevel 2))
    
    Although I have set `org-md-toplevel-hlevel` to `2` to fulfill the
    demotion of headlines during export, it is still possible to alter this
    behavior in the necessary documents through the BIND keyword.


### Writing with Markdown

Markdown<sup><a id="fnr.16" class="footref" href="#fn.16" role="doc-backlink">16</a></sup>, perhaps the most prevalent text markup language of the first
half of the 21st century, is utilized by virtually all open-source
developers. I, unable to escape the trend, occasionally find myself
editing these documents.


## Programming

Emacs is an exceptionally potent programming environment. However, it
necessitates meticulous configuration; otherwise, its functioning will
be subpar.  Let me to tailor Emacs to accommodate all the programming
languages I used.

    (use-package markdown-mode
      :ensure markdown-mode
      :mode
      ("\\.markdown\\'" . markdown-mode)
      ("\\.md\\'" . markdown-mode)
      ("\\.mdown\\'" . markdown-mode)
      ("\\.mkd\\'" . markdown-mode)
      ("\\.mkdn\\'" . markdown-mode)
      ("README\\.md\\'" . gfm-mode)
      :config
      (setq markdown-command "multimarkdown"))


### Emacs Lisp

When crafting Emacs Lisp programs, we often write and invoke numerous
macros.  Merely contemplating to decipher the code generated by macros
is insufficient.  I used `pp-macroexpand-last-sexp` to preview the
current macro expansion.  For convenience, it is bound to the <kbd> C-c C-v </kbd> key in **Emacs Lisp Mode**.  Here, <kbd> v </kbd> implies verbose.

    (use-package pp
      :keymap-set
      (:emacs-lisp-mode-map
       ("C-c C-v" . pp-macroexpand-last-sexp)))


### Nix

By default, Emacs does not furnish **Nix Mode**.  Consequently, it
necessitates installation from ELPA, followed by automatic activation
predicated on the extension name.

    (use-package nix-mode
      :ensure nix-mode
      :mode
      ("\\.nix\\'" . nix-mode))


### YAML

YAML<sup><a id="fnr.17" class="footref" href="#fn.17" role="doc-backlink">17</a></sup> is an exceedingly prevalent configuration language. Despite my
personal aversion towards it, I have to incorporate its support.

    (use-package yaml-mode
      :ensure yaml-mode
      :config
      (keymap-set yaml-mode-map "C-m" 'newline-and-indent)
      :mode
      ("\\.yaml\\'" . yaml-mode)
      ("\\.yml\\'" . yaml-mode))

Unlike `python-mode`, this mode follows the Emacs convention of not
binding the <kbd> ENTER </kbd> key to `newline-and-indent`.  To get this
behavior, bind it in `yaml-mode`.


## Footnotes

<sup><a id="fn.1" href="#fnr.1">1</a></sup> early-init, <early-init.md>

<sup><a id="fn.2" href="#fnr.2">2</a></sup> Twist, <https://github.com/emacs-twist/twist.nix>

<sup><a id="fn.3" href="#fnr.3">3</a></sup> GCMH Mode, <https://gitlab.com/koral/gcmh>

<sup><a id="fn.4" href="#fnr.4">4</a></sup> Moody Mode Line, <https://github.com/tarsius/moody>

<sup><a id="fn.5" href="#fnr.5">5</a></sup> Minions, <https://github.com/tarsius/minions>

<sup><a id="fn.6" href="#fnr.6">6</a></sup> Modus Themes, <https://protesilaos.com/emacs/modus-themes>

<sup><a id="fn.7" href="#fnr.7">7</a></sup> Switch Window, <https://github.com/dimitri/switch-window>

<sup><a id="fn.8" href="#fnr.8">8</a></sup> Vertico, <https://github.com/minad/vertico>

<sup><a id="fn.9" href="#fnr.9">9</a></sup> Orderless, <https://github.com/oantolin/orderless>

<sup><a id="fn.10" href="#fnr.10">10</a></sup> Marginalia, <https://github.com/minad/marginalia>

<sup><a id="fn.11" href="#fnr.11">11</a></sup> Magit, <https://magit.vc>

<sup><a id="fn.12" href="#fnr.12">12</a></sup> Nix3.el, <https://github.com/emacs-twist/nix3.el>

<sup><a id="fn.13" href="#fnr.13">13</a></sup> Line Wrap, <https://www.emacswiki.org/emacs/LineWrap>

<sup><a id="fn.14" href="#fnr.14">14</a></sup> Modern Org Style, <https://github.com/minad/org-modern>

<sup><a id="fn.15" href="#fnr.15">15</a></sup> Org Side Tree, <https://github.com/localauthor/org-side-tree>

<sup><a id="fn.16" href="#fnr.16">16</a></sup> Markdown, <https://daringfireball.net/projects/markdown/>

<sup><a id="fn.17" href="#fnr.17">17</a></sup> YAML, <https://yaml.org/>
