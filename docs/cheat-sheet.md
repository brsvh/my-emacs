# My Emacs Cheat Sheet

Here, I describe my custom keybinding settings. There are two guiding principles for the design of the bindings:

- Adhere to the conventional bindings of the Emacs Realm.
- Follow the initial letter of the action name.

## `global-map`

| Key                    | Command                                          | Action                                                            | Provider           |
|------------------------|--------------------------------------------------|-------------------------------------------------------------------|--------------------|
| <kbd>C-a</kbd>         | `mwim-beginning-of-code-or-line`                 | Move to the beginning of code or line                             | `mwim`             |
| <kbd>C-c HOME r</kbd>  | `restart-emacs`                                  | Restart Emacs                                                     | `files`            |
| <kbd>C-c a n</kbd>     | `org-roam-capture`                               | Capture a note                                                    | `org-roam-capture` |
| <kbd>C-c a s</kbd>     | `org-store-link`                                 | Store current position as org link                                | `ol`               |
| <kbd>C-c e C-SPC</kbd> | `pangu-spacing-mode`                             | Insert spaces at the junction between Chinese and other languages | `pangu-spacing`    |
| <kbd>C-c e SPC</kbd>   | `my/current-buffer-delete-trailing-whitespace`   | Delete trailing spaces in current buffer                          | `my-editor`        |
| <kbd>C-c e TAB</kbd>   | `my/current-buffer-untabify`                     | Untabify current buffer                                           | `my-editor`        |
| <kbd>C-c f r</kbd>     | `consult-recent-file`                            | Interactively open recented files                                 | `consult`          |
| <kbd>C-c p TAB C</kbd> | `tabspaces-clear-buffers`                        | Clear all buffers in current project tab                          | `tabspaces`        |
| <kbd>C-c p TAB R</kbd> | `tabspaces-remove-selected-buffer`               | Remove selected buffer from current project tab                   | `tabspaces`        |
| <kbd>C-c p TAB S</kbd> | `tabspaces-switch-buffer-and-tab`                | Switch to buffer, from all project tabs                           | `tabspaces`        |
| <kbd>C-c p TAB b</kbd> | `tabspaces-switch-or-create-workspace`           | Switch to or create a project tab                                 | `tabspaces`        |
| <kbd>C-c p TAB d</kbd> | `tabspaces-close-workspace`                      | Close current project tab                                         | `tabspaces`        |
| <kbd>C-c p TAB k</kbd> | `tabspaces-kill-buffers-close-workspace`         | Close current project tab with kill buffers                       | `tabspaces`        |
| <kbd>C-c p TAB o</kbd> | `tabspaces-open-or-create-project-and-workspace` | Open a directory as project tab                                   | `tabspaces`        |
| <kbd>C-c p TAB r</kbd> | `tabspaces-remove-current-buffer`                | Remove current buffer from current project tab                    | `tabspaces`        |
| <kbd>C-c p TAB s</kbd> | `tabspaces-switch-to-buffer`                     | Switch to buffer, from current project tab                        | `tabspaces`        |
| <kbd>C-c s S</kbd>     | `rg-save-search-as-name`                         | Save searched result in current buffer as file                    | `rg`               |
| <kbd>C-c s d</kbd>     | `rg-dwim`                                        | Run `ripgrep`.                                                    | `rg`               |
| <kbd>C-c s k</kbd>     | `rg-kill-saved-searches`                         | Kill all saved `ripgrep` results                                  | `rg`               |
| <kbd>C-c s l</kbd>     | `rg-list-searches`                               | List all searched `ripgrep` results                               | `rg`               |
| <kbd>C-c s p</kbd>     | `rg-project`                                     | Run `ripgrep` in current project                                  | `rg`               |
| <kbd>C-c s r</kbd>     | `rg`                                             | Run `ripgrep` with REGEXP                                         | `rg`               |
| <kbd>C-c s s</kbd>     | `rg-save-search`                                 | Save searched result in current buffer                            | `rg`               |
| <kbd>C-c s t</kbd>     | `rg-literal`                                     | Run `ripgrep` with PATTERN                                        | `rg`               |
| <kbd>C-c v g d</kbd>   | `magit-dispatch`                                 | Git operations for current directory                              | `magit`            |
| <kbd>C-c v g s</kbd>   | `magit-status`                                   | Git status of current directory                                   | `magit`            |
| <kbd>C-e</kbd>         | `mwim-end-of-code-or-line`                       | Move to the end of code or line                                   | `mwim`             |
| <kbd>C-x r l</kbd>     | `consult-bookmark`                               | List bookmarks                                                    | `consult`          |
| <kbd>C-y</kbd>         | `consult-yank-from-kill-ring`                    | Paste from kill ring                                              | `consult`          |
| <kbd>M-y</kbd>         | `consult-yank-pop`                               | Paste and pop from kill ring                                      | `consult`          |

## Builtin features

### Dired (`dired-mode-map`)

| Key          | Command               | Action                            | Provider         |
|--------------|-----------------------|-----------------------------------|------------------|
| <kbd>)</kbd> | `dired-git-info-mode` | Show Git info of opened directory | `dired-git-info` |

## Programming modes

| Key              | Command           | Action                                 | Provider |
|------------------|-------------------|----------------------------------------|----------|
| <kbd>C-c !</kbd> | `consult-flymake` | Show all diagnostics of current buffer | `consult |

### Emacs Lisp (`emacs-lisp-mode`)

| Key       | Command                    | Action                     | Provider |
|-----------|----------------------------|----------------------------|----------|
| <kbd>C-c C-v</kbd> | `pp-macroexpand-last-sexp` | Expand macro body at point | `pp`     |

## Writing modes

### Markdown (`markdown-mode`)

| Key         | Command     | Action                                           | Provider    |
|-------------|-------------|--------------------------------------------------|-------------|
| <kbd>C-c C-c g</kbd> | `grip-mode` | Run livepreview server for current Markdown file | `grip-mode` |

### `org-mode`

| Key                | Command                  | Action                               | Provider        |
|--------------------|--------------------------|--------------------------------------|-----------------|
| <kbd>C-c c i</kbd> | `org-cite-insert`        | Insert a citation                    | `oc`            |
| <kbd>C-c m t</kbd> | `org-side-tree`          | Open a outline tree at left side     | `org-side-tree` |
| <kbd>C-c r b</kbd> | `org-roam-buffer-toggle` | Open Org Roam buffer of current node | `org-roam-mode` |
| <kbd>C-c r f</kbd> | `org-roam-node-find`     | Search and open a Org Roam node      | `org-roam-node` |
| <kbd>C-c r i</kbd> | `org-roam-node-insert`   | Insert an existed Org Roam node      | `org-roam-node` |
