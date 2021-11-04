# org-nanowrimo-setup-mode
A basic mode to setup a frame for nanowrimo editing, outline left side, editing right side, accurate wordcount on plain text export

This is not a real solid package, this is just me breaking out my nanowrimo-setup.  Essentially you specify an org file for your nanowrimo project then when you open that file it takes over the frame, splits it into two windows, makes the left hand window a structure overview of just headlines using ```outline-hide-body```, and makes the right hand window an editing window narrowed down to the last bit of content seen using ```org-tree-to-indirect-buffer```.

When you save the file it exports the bodies of all the trees and filters out all the headlines, dumping it to a .txt file version of the .org file, then it counts the words for that.  It also removes any subtree that is tagged with `:notes:` so you can use this to either comment out a subtree (be that a chapter or scene) or just have a whole tree of notes.

You can toggle the visability of the outline window using ```org-nanowrimo-setup-outline-window-toggle```.

When you save it will output a message charting your progress, by default this looks like:
```Wordcount A, B (above|below) goal C (Need D avg | Remaining: E words, F days)```

Which allows you to see how many words you've got (A) and if you're hitting the general goal for the whole timespan (C) or how much you're above or below it (B), and also shows you the average amount of words you need to write daily (D) to hit your remaining word count (E) over the remaining days (F).  For example:

```Wordcount 25038, 1714 above goal 23324 (Need 1560 avg | Remaining: 24962 words, 16 days)```

If you want to log your wordcount for every save to a file then check ```org-nanowrimo-setup-log-wordcounts```, if this is set to t then it takes your ```org-nanowrimo-setup-path``` file, drops the .org from the end and substitutes .stats and writes the timestamp and word count in that in the format:

```YYYY-MM-DD HH-MM-SS word count: NNNN```

There's a bunch of configurable options documented in it go have a poke.

# Workflow

Basically get the package configured, then open up your nano.org file, your frame will split and any other config you wanted will appply.  If you have `save-place-mode` enabled it should restore where you last were.  The left hand window is your outline, just the headlines of your org file, the right hand window is for editing, by default it will show the last heading you were on.

If you want to change your heading select it from the left hand window and hit `C-c C-x b` to trigger `org-tree-to-indirect-buffer` which will open in the right.  You can open either just one small section, a whole chapter, or a whole story tree at once.  Then flip back to the right hand window and edit away, you can fold/unfold trees as normal in org using the `<tab>` key.  I often have a chapter open at a time and can always change where I'm looking by going to the org headings on the left hand side and using `C-c C-x b` again to open a different part of the tree in the right hand editing buffer, this will remove the previous one from your buffer list in the default config.

I usually use `C-c C-x n` to trigger `org-nanowrimo-setup-tree-to-indirect-frame` and have a second Emacs frame purely with my notes in so I can refer to those easily.

Sometimes I'll want a specific tag (I'm tagging on names and places) so I'll use `C-c \` to trigger `org-match-sparse-tree` and narrow my view of the tree down to just the relevant segments.

# Example config:
```
(use-package org)
(use-package org-nanowrimo-setup-mode
    :after org

    :init
    (setq org-nanowrimo-setup-path (expand-file-name "~/nano2021/nano.org"))
    
    :config
    (org-nanowrimo-setup-mode))
```

If you don't want it attempting to fullscreen your emacs use:
```
(remove-hook 'org-nanowrimo-setup-gui-adjustments-hook #'toggle-frame-fullscreen)
```

If you want to change the default size of the left hand outline panel then you can change that, default value is 40 which is chosen to work for my fonts.
```
(setq org-nanowrimo-setup-initial-sidebar-size X)
```


# More detailed example config:

This example is sort of glitchy but working.  Essentially switches the theme for the whole emacs over to running the farmhouse-dark theme to make a contrast to day-to-day editing and get my brain into writing mode.  Also switches off company-mode because it bugs me if I'm typing and trying to think of a word I want.  Finally it switches on darkroom-mode but only for the editing buffer.

You can also open additional frames with `org-nanowrimo-setup-tree-to-indirect-frame` which is bound to `C-c C-x n` here, this is handy for having a second frame open on an org-tree of notes (places, characters, etc)

You can show and hide the outline/structure window using `C-c C-x o` which is bound to `org-nanowrimo-setup-outline-window-toggle`.

I've also got `C-c C-x l` bound to `org-nanowrimo-setup-refresh-outline` which fixes the outline view if it gets messed up without interrupting your flow

```
(use-package org-nanowrimo-setup-mode
  :after org
  :init
  (setq org-nanowrimo-setup-path (expand-file-name "~/nano2021/nano.org")
        org-nanowrimo-setup-log-wordcounts t
        org-nanowrimo-setup-initial-sidebar-size 20
        org-nanowrimo-setup-late-night-theme-start-hour 18
        org-nanowrimo-setup-late-night-theme-end-hour 7)

  (defun my/org-nanowrimo-setup-late-night-theme-p ()
    "Tests if we should be using the late night theme, relies on checking the variables org-nanowrimo-setup-late-night-theme-start-hour and org-nanowrimo-setup-late-night-theme-end-hour against the current hour, so setting these to 18 and 7 respectively means your late night theme will be switched to for all editing sessions started between 18:00 and 07:00 in the morning"
    (interactive)
    (let ((curr-hour (string-to-number
                      (format-time-string "%H" (current-time)))))
      (or (>= curr-hour org-nanowrimo-setup-late-night-theme-start-hour)
          (<= curr-hour org-nanowrimo-setup-late-night-theme-end-hour))))

  ;; Outline (left hand pane) tests/adjustments
  (add-hook 'org-nanowrimo-setup-reapply-outline-adjustments-hook
            (lambda ()
              (if (my/org-nanowrimo-setup-late-night-theme-p)
                  (progn
                    (set-face-background 'hl-line "grey10")
                    (set-face-attribute 'isearch nil :background "DarkGreen")))))

  ;; Editing (right hand pane) tests and adjustments
  (add-hook 'org-nanowrimo-setup-reapply-editing-buffer-test-function
            (lambda () (not darkroom-mode)))

  (add-hook 'org-nanowrimo-setup-reapply-editing-adjustments-hook
            (lambda ()
              (progn
                ;; (load-theme-buffer-local 'farmhouse-dark (current-buffer))
                ;; (centered-cursor-mode)
                (company-mode -1)
                (make-local-variable 'scroll-margin)
                (setq scroll-margin 5)
                (if (not darkroom-mode)
                    (darkroom-mode 1)))))

  ;; Theme tweaks/changes
  (add-hook 'org-nanowrimo-setup-theme-adjustments-hook
            (lambda ()
              (progn
                ;; Load a custom theme based on time if 19:00 or past,
                ;; but also tweak some of the colour scheme
                (if (my/org-nanowrimo-setup-late-night-theme-p)
                    (progn 
                      (if custom-enabled-themes
                          (disable-theme custom-enabled-themes))
                      (load-theme 'farmhouse-dark)
                      (set-face-attribute 'hl-line nil
                                          :inherit nil
                                          :background "gray10")))
                (company-mode -1))))

  ;; Setting up binds by hand in the :config section to avoid messing
  ;; up the org binds on machines where the file is not present for
  ;; shipping this config around.
  ;;
  ;; :bind (:map org-mode-map (("C-c C-x l" . (my/org-nanowrimo-setup-mode-refresh-outline))))

  :config
  (if (file-exists-p org-nanowrimo-setup-path)
      (progn
        
        ;; Keybinds only being put in place if the file exists.
        (define-key org-mode-map (kbd "C-c C-x n")
          'org-nanowrimo-setup-tree-to-indirect-frame)
        (define-key org-mode-map (kbd "C-c C-x o")
          'org-nanowrimo-setup-outline-window-toggle)
        (define-key org-mode-map (kbd "C-c C-x l")
          'org-nanowrimo-setup-refresh-outline)

        ;; Similarly only require packages if file exists
        (use-package darkroom
          :ensure t
          :init (setq darkroom-text-scale-increase 0.8))
        (use-package "farmhouse-theme"
          :ensure t
          :defer t)

        ;; Switch mode on
        (org-nanowrimo-setup-mode))))
```

# Writing at a different time to nanowrimo?  Have a different goal from 50k?

Check the variables `org-nanowrimo-setup-start-date` and `org-nanowrimo-setup-end-date` to set the times of your writing, they default to all of November in whatever the current year is.  If you're word goal is different set `org-nanowrimo-setup-word-goal`, your daily goals will be calculated from these three.

# Example nano.org file

Using tags means you can use `org-match-sparse-tree` to narrow things down to just those tags.  Tags inherit so the whole top tree never appears in your output and wordcount, nor does the node "*** Background info" because they're flagged as `:notes:`

```
* Notes :notes:
** Characters
*** Strange Figure
:END:
*** A ghost
:END:
** Places
*** A big house
:END:
*** A spooky forest
:END:
** Plot points
*** Remember to include a wolf
:END:

* Story
** Chapter One :woods:ghost:
*** Scene One
:END:
*** Scene Two
:END:
*** Background info :notes:
:END:

** Chapter Two :house:strangeFigure:
*** Scene One
:END:
*** Old Scene One :notes:
:END:
*** Scene Two
:END:

** Chapter Three :ghost:woods:strangeFigure:
*** Scene One
:END:
*** Scene Two
:END:
```

# Bugs:

This mode relies on ```outline-hide-body``` to setup its sidebar to only show headings in its left pane and ```org-tree-to-indirect-buffer``` to only show the section you're editing in the right pane.  However this means if you are adding things to the end of the edited section (the indirect buffer) then you will get updates appearing in the left pane after the ... that ```outline-hide-body``` leaves at the end of its narrowed regions, since these exist between the narrowed selections (I think) they appear straight in the buffer.

The best way to avoid this is to end every subtree with an [Org drawer](https://orgmode.org/manual/Drawers.html) on its own, then you'll always be editing above that, and hence not messing up the outline view, so at the end of chapters write ```:END:``` and that'll fix it, it also will get filtered out by the Org to text output for your wordcount and not effect it, this is included in the example above, but explained here.

```
* Story :story:
** Chapter One
  And all your writing goes here, so you're always inside the hidden region.
:END:
** Chapter Two
  The second chapter goes here likewise.  Thanks weird buggy outline mode.
:END:
```

# Other resources:

In making this and generally dinking about using Emacs for writing I was inspired by the following articles and ideas:

 * https://tonyballantyne.com/EmacsWritingTips.html
 * https://vimvalley.com/replacing-scrivener-with-emacs-and-vim/
 * https://www.tomheon.com/2019/04/10/how-an-uber-geeky-text-mode-in-a-40-year-old-editor-saved-my-novel/
 * https://www.reddit.com/r/orgmode/comments/ae5str/using_orgmode_for_novel_writingcreative_writing/
 * Jay Dixit's "Emacs for Writers" video (1:01:05 long) https://www.youtube.com/watch?v=FtieBc3KptU
