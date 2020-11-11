# org-nanowrimo-setup-mode
A basic mode to setup a frame for nanorimo editing, outline left side, editing right side, accurate wordcount on plain text export

This is not a real solid package, this is just me breaking out my nanowrimo-setup.  Essentially you specify an org file for your nanowrimo project then when you open that file it takes over the frame, splits it into two windows, makes the left hand window a structure overview of just headlines, and makes the right hand window an editing window narrowed down to the last bit of content seen using org-tree-to-indirect-buffer.

When you save the file it exports the bodies of all the trees and filters out all the headlines, dumping it to a .txt file version of the .org file, then it counts the words for that.  It also removes any subtree that is tagged with `:notes:` so you can use this to either comment out a subtree (be that a chapter or scene) or just have a whole tree of notes.

There's a bunch of configurable options documented in it go have a poke.

# Example config:
```
(use-package org)
(use-package org-nanowrimo-setup-mode
    :after org

    :init
    (setq org-nanowrimo-setup-path (expand-file-name "~/nano2020/nano.org"))
    
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

You can also open additional frames with `org-nanowrimo-setup-tree-to-indirect-frame` which is bound to "C-c C-x n" here, this is handy for having a second frame open on an org-tree of notes (places, characters, etc)

```
(use-package org)
(use-package nanowrimo-setup-mode
  :after org
  :init
  (setq org-nanowrimo-setup-path (expand-file-name
                           "~/nano2020/nano.org"))

  ;; Outline (left hand pane) tests/adjustments
  (add-hook 'org-nanowrimo-setup-reapply-outline-adjustments-hook
            (lambda ()
              (set-face-background 'hl-line "grey10")
              (set-face-attribute 'isearch nil :background "DarkGreen")))

  ;; Editing (right hand pane) tests and adjustments
  (add-hook 'org-nanowrimo-setup-reapply-editing-buffer-test-function
            (lambda () (not darkroom-mode)))

  (add-hook 'org-nanowrimo-setup-reapply-editing-adjustments-hook
            (lambda ()
              (progn
                (company-mode -1)
                (make-local-variable 'scroll-margin)
                (setq scroll-margin 5)
                (if (not darkroom-mode)
                    (darkroom-mode)))))

  ;; Theme tweaks/changes
  (add-hook 'org-nanowrimo-setup-theme-adjustments-hook
            (lambda ()
              (progn
                (load-theme 'farmhouse-dark)
                (set-face-attribute 'hl-line nil
                                    :inherit nil
                                    :background "gray10")
                (company-mode -1))))
  
  :config
  (if (file-exists-p org-nanowrimo-setup-path)
      (progn
        (define-key org-mode-map (kbd "C-c C-x n")
          'org-nanowrimo-setup-tree-to-indirect-frame)
        (use-package darkroom
          :ensure t
          :init (setq darkroom-text-scale-increase 0.6))
        (use-package "farmhouse-theme"
          :ensure t
          :defer t)
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
*** A ghost
** Places
*** A big house
*** A spooky forest
** Plot points
*** Remember to include a wolf

* Story
** Chapter One :woods:ghost:
*** Scene One
*** Scene Two
*** Background info :notes:

** Chapter Two :house:strangeFigure:
*** Scene One
*** Old Scene One :notes:
*** Scene Two

** Chapter Three :ghost:woods:strangeFigure:
*** Scene One
*** Scene Two
```
