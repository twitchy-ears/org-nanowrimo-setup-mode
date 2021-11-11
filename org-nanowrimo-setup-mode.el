;;; org-nanowrimo-setup-mode.el --- Minor mode for nanorimo editing with outline, editing, and accurate wordcount

;; Copyright 2020, 2021 - Twitchy Ears

;; Author: Twitchy Ears https://github.com/twitchy-ears/
;; URL: https://github.com/twitchy-ears/org-nanowrimo-setup-mode
;; Version: 0.2
;; Package-Requires ((emacs "26.1") (org "9") (seq) (save-place) cl-lib)
;; Keywords: org nanowrimo convienience frames outlines

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;; This is not a real solid package, this is just me breaking out my
;; nanowrimo-setup it does include accurate to wc(1) word counting, a
;; split outline/editing view and plain text output (part of the
;; wordcounting)

;;; History
;;
;; 2020-11-11 - initial version
;; 2021-11-04 - a few small tidy ups, optional wordcount logging,
;;              and improved documentation including the note to use
;;              :END: property drawers at the end of your chapters and
;;              things to stop the outline getting messed up when
;;              you're adding text above it.
;; 2021-11-11 - Fix of editing buffer opening code so it doesn't stamp
;;              on additional buffers, cleaned up with flycheck, customisable
;;              message output for wordcounts, etc.

;;; Commentary:

;; Essentially all you need is this, be aware that
;; ~/nano2021/nano.txt will be created for word counting/export
;; purposes:
;; 
;; (use-package org)
;; (use-package org-nanowrimo-setup-mode
;;     :after org
;;     :init
;;     (setq org-nanowrimo-setup-path (expand-file-name "~/nano2021/nano.org"))
;;     :config
;;     (org-nanowrimo-setup-mode))
;;
;;
;;
;; A longer example includes:
;; (use-package org)
;; (use-package org-nanowrimo-setup-mode
;;   :after org
;;   :init
;;   (setq org-nanowrimo-setup-path (expand-file-name "~/nano2021/nano.org")
;;         org-nanowrimo-setup-log-wordcounts t
;;         org-nanowrimo-setup-initial-sidebar-size 20
;;         org-nanowrimo-setup-late-night-theme-start-hour 18
;;         org-nanowrimo-setup-late-night-theme-end-hour 7)
;; 
;;   (defun my/org-nanowrimo-setup-late-night-theme-p ()
;;     "Tests if we should be using the late night theme, relies on checking the variables org-nanowrimo-setup-late-night-theme-start-hour and org-nanowrimo-setup-late-night-theme-end-hour against the current hour, so setting these to 18 and 7 respectively means your late night theme will be switched to for all editing sessions started between 18:00 and 07:00 in the morning"
;;     (interactive)
;;     (let ((curr-hour (string-to-number
;;                       (format-time-string "%H" (current-time)))))
;;       (or (>= curr-hour org-nanowrimo-setup-late-night-theme-start-hour)
;;           (<= curr-hour org-nanowrimo-setup-late-night-theme-end-hour))))
;; 
;;   ;; Outline (left hand pane) tests/adjustments
;;   (add-hook 'org-nanowrimo-setup-reapply-outline-adjustments-hook
;;             (lambda ()
;;               (if (my/org-nanowrimo-setup-late-night-theme-p)
;;                   (progn
;;                     (set-face-background 'hl-line "grey10")
;;                     (set-face-attribute 'isearch nil :background "DarkGreen")))))
;; 
;;   ;; Editing (right hand pane) tests and adjustments
;;   (add-hook 'org-nanowrimo-setup-reapply-editing-buffer-test-function
;;             (lambda () (not darkroom-mode)))
;; 
;;   (add-hook 'org-nanowrimo-setup-reapply-editing-adjustments-hook
;;             (lambda ()
;;               (progn
;;                 ;; (load-theme-buffer-local 'farmhouse-dark (current-buffer))
;;                 ;; (centered-cursor-mode)
;;                 (company-mode -1)
;;                 (make-local-variable 'scroll-margin)
;;                 (setq scroll-margin 5)
;;                 (if (not darkroom-mode)
;;                     (darkroom-mode 1)))))
;; 
;;   ;; Theme tweaks/changes
;;   (add-hook 'org-nanowrimo-setup-theme-adjustments-hook
;;             (lambda ()
;;               (progn
;;                 ;; Load a custom theme based on time if 19:00 or past,
;;                 ;; but also tweak some of the colour scheme
;;                 (if (my/org-nanowrimo-setup-late-night-theme-p)
;;                     (progn
;;                       (if custom-enabled-themes
;;                           (disable-theme custom-enabled-themes))
;;                       (load-theme 'farmhouse-dark)
;;                       (set-face-attribute 'hl-line nil
;;                                           :inherit nil
;;                                           :background "gray10")))
;;                 (company-mode -1))))
;; 
;;   :config
;;   (if (file-exists-p org-nanowrimo-setup-path)
;;       (progn
;; 
;;         ;; Only require packages if file exists
;;         (use-package darkroom
;;           :ensure t
;;           :init (setq darkroom-text-scale-increase 0.8))
;;         (use-package "farmhouse-theme"
;;           :ensure t
;;           :defer t)
;; 
;;         ;; Switch mode on
;;         (org-nanowrimo-setup-mode))))
;;
;;
;; It should be noted that this will take over the frame its in and
;; cause some havoc in its wake (fullscreening stuff creating splits,
;; and optionally changing themes around) it will also export a plain
;; text version to "~/nano2021/nano.txt" every time you write a change
;; to the .org file, and append a line with your word count in and the
;; timestamp to "~/nano2021/nano.stats", this allows you to have
;; accurate wordcount without any weird forms of filtering the current
;; buffer

;; TODO:
;;
;; Look at using mouse-3 on the org-mode headers to open them in the editing window perhaps?
;; 
;; This should probably be looking at org-sidebar to see if
;; that would do anything more useful for me:
;; https://github.com/alphapapa/org-sidebar
;;
;; Also maybe transclusion stuff instead of opening indirect buffers or to break things up across multiple org files? see https://github.com/nobiot/org-transclusion
;;
;; Think about frequency analysis https://emacs.stackexchange.com/questions/13514/how-to-obtain-the-statistic-of-the-the-frequency-of-words-in-a-buffer

;;; Code:
(eval-and-compile
  (require 'org)
  (require 'ox)
  (require 'ox-ascii)
  (require 'org-element)
  (require 'seq)
  (require 'saveplace)
  (with-no-warnings (require 'cl-lib)))

(defvar org-nanowrimo-setup-path nil "Path to the .org file where your nanowrimo project will be stored.  This should point to an existing file to trigger the creation of the org-mode-hook that sets this all up.")

(defvar org-nanowrimo-setup-have-configured-frame nil "Is set to t once a frame has been configured with the org-nanowrimo-setup-configure-window function, this reduces frame mangling on revisiting.")

(defvar org-nanowrimo-setup-initial-sidebar-size 40 "Size of the lefthand sidebar.")
(defvar org-nanowrimo-setup-lock-outline-window-size t "Fix the outline windows width.")
(defvar org-nanowrimo-setup-outline-window-id nil "Stores the outline window ID.")
(defvar org-nanowrimo-setup-editing-window-id nil "Stores the editing window ID.")
(defvar org-nanowrimo-setup-window-configuration nil "Stores the default window configuration.")

(defvar org-nanowrimo-setup-export-filtered-tags "+notes" "Org tag selection string for tagged trees to filter out of the export, see: https://orgmode.org/manual/Matching-tags-and-properties.html.")

(defvar org-nanowrimo-setup-export-filter-headlines t "If headlines should be filtered out of the export.")

(defvar org-nanowrimo-setup-export-replace-headlines-str "" "String to replace headlines with.")

(defvar org-nanowrimo-setup-export-message-function #'message
  "Function run by 'org-nanowrimo-setup-export-txt' to display wordcount.
Defaults to 'message'.

Will be fed an argument of a string describing the current wordcount and
optionally goals based on other variables, see
'org-nanowrimo-setup-export-count-words',
'org-nanowrimo-setup-export-show-goal', and
'org-nanowrimo-setup-export-show-moving-goal'.")

(defvar org-nanowrimo-setup-theme-adjustments-hook nil "Hook to run before checking for GUI to make any theme tweaks/changes you want.")

(defvar org-nanowrimo-setup-gui-adjustments-hook
  nil
  "Hook run if org-nanowrimo-setup-configure-window is inside a GUI.
As tested by (window-system).  Alternative ideas include just resizing the
frame: (lambda () (set-frame-size (selected-frame) 160 45))")
(add-hook 'org-nanowrimo-setup-gui-adjustments-hook #'toggle-frame-fullscreen)

(defvar org-nanowrimo-setup-reapply-editing-buffer-test-function nil "Hook that runs when testing to see if org-nanowrimo-setup-reapply-editing should run, useful to test to see if a mode is already enabled if you want to call a toggle.  Every hooked function must return t.")

(defvar org-nanowrimo-setup-reapply-editing-adjustments-hook nil "Additional code to run when adjusting the editing right hand frame, this will run whenever that frame is entered due to the hook, so be careful.")

(defvar org-nanowrimo-setup-reapply-outline-buffer-test-function nil "Hook that runs when testing to see if org-nanowrimo-setup-reapply-outline should be run, useful to test to see if a mode is already enabled if you want to call a toggle.  Every hooked function must return t.")

(defvar org-nanowrimo-setup-reapply-outline-adjustments-hook nil "Hook that runs when adjusting the outline left hand frame, this will run whenever that frame is entered due to the hook, so be careful.")

(defvar org-nanowrimo-setup-switch-off-adjustments-hook nil "Additional code to run when switching the mode off if it has been enabled, so reverting themes and so forth.  Expects a lambda.")

(defvar org-nanowrimo-setup-export-count-words t "Show words after export.")
(defvar org-nanowrimo-setup-export-show-goal t "Calculate daily goals and show how export is doing for under/over shooting this, this is on average over the whole time, if you don't want recalculated goals you need to set org-nanowrimo-setup-export-show-moving-goals to nil.")
(defvar org-nanowrimo-setup-export-show-moving-goal t "Calculate daily average needed based on how much wordcount is left vs how many days are left.  Set to nil to just show goals over the whole timespan.")

(defvar org-nanowrimo-setup-log-wordcounts nil "Will create an output file taking the base name of your .org file but with the suffix .stats, will store the date/time to saves and the word count in there.")

;; See also (org-time-convert-to-integer
;;              (org-nanowrimo-setup-ymd-to-encode-time "2020-11-01"))
(defun org-nanowrimo-setup-ymd-to-encode-time (ymd)
  "Expects a string argument (YMD) in the form YYYY-MM-DD and will return an Emacs time string structure in response which can then be fed into 'format-time-string' or 'time-to-days' or other functions."
  (let ((p (parse-time-string ymd)))
    (encode-time 0 0 0 (nth 3 p) (nth 4 p) (nth 5 p))))

(defvar org-nanowrimo-setup-start-date
  (org-nanowrimo-setup-ymd-to-encode-time (format-time-string "%Y-11-00" (current-time)))
  "Start date of the writing challenge, defaults to November of the current year.")

(defvar org-nanowrimo-setup-end-date
  (org-nanowrimo-setup-ymd-to-encode-time (format-time-string "%Y-11-30" (current-time)))
  "End date of the writing challenge, defaults to 30th November of the current year.")

(defvar org-nanowrimo-setup-word-goal 50000 "Number of words you're aiming to hit between org-nanowrimo-setup-start-date and org-nanowrimo-setup-end-date.")

(defvar org-nanowrimo-setup-todays-goal nil "Contains the calculated goal for a day based on wordcount/length.")
(defvar org-nanowrimo-setup-days-remaining nil "Contains the calculated number of days left for working out the average remaining needed.")
(defvar org-nanowrimo-setup-goal-update-timer nil "Holds the timer that is used to refresh the word count.")

(defvar org-nanowrimo-setup-editing-buffer-name nil "Holds the current buffer name for the buffer being edited in 'org-nanowrimo-setup-editing-window-id'.")



(defun org-nanowrimo-setup-org-get-tags ()
  "Dump out the tags at point as a message in the minibuffer, essentially just a wrapper with (interactive)."
  (interactive)
  (message "%s" (org-get-tags)))

(defun org-nanowrimo-setup-count-words (start end)
  "Inspired by simple.el version of 'count-words' but will match the count used by wc(1) because its counting whitespaces rather than 'forward-word' or 'forward-symbol'.  This will take two arguments START and END for a region when called from elisp or when called interactively expects a region.  Will return the count but outputs a message as well if called interactively."
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (goto-char (point-min))
      (let ((words (count-matches search-whitespace-regexp)))
        (if (called-interactively-p 'any)
            (progn
              (message "Words: %d" words)
              words)
          words)))))

(defun org-nanowrimo-setup-tree-to-indirect-frame (&optional ARG)
  "Wraps 'org-tree-to-indirect-buffer' and passes ARG to it, while setting 'org-indirect-buffer-display' to 'new-frame' so it opens in a fresh frame."
  (interactive)
  (let ((org-indirect-buffer-display 'new-frame))
    (org-tree-to-indirect-buffer ARG)))

(defun org-nanowrimo-setup-tree-to-editing-buffer (&optional ARG)
  "Rework of 'org-tree-to-indirect-buffer' that maintains one editing buffer.
It closes the current indirect buffer in the right hand editing pane and opens
a fresh indirect buffer over the top of whatevers in the editing pane,
which is positioned on the right hand side.

Takes an optional ARG and ignores it."
  (interactive)
      (let* ((cbuf (current-buffer))
             (cwin (selected-window))
             (origpos (point))
             (heading nil)
             (newbuf nil)
             (newbuf-name nil))

        ;; Get the details inside a save-excursion/restriction as this
        ;; means moving around in the outline buffer and we'd like to
        ;; prevent this, however we *do* want to move around in the
        ;; rest of the function so this is the only bit in a save.
        (save-excursion
          (save-restriction
            (setq
                  heading (org-get-heading 'no-tags)
                  
                  newbuf-name (generate-new-buffer-name
                               (format "Indirect %s - %s"
                                       (buffer-name cbuf)
                                       heading))
                  newbuf (make-indirect-buffer cbuf newbuf-name t))))
        
        ;; Hop to our editing window
        (if org-nanowrimo-setup-editing-window-id
            (select-window org-nanowrimo-setup-editing-window-id)
          (other-window 1))

        ;; Switch the editing window over to looking at the new
        ;; buffer, make sure its narrowed correctly, so widen to all,
        ;; then restrict to the current subtree and make sure you show
        ;; it or it inherits being folded by the outline view.
        (switch-to-buffer newbuf)
        (widen)
        (org-narrow-to-subtree)
        (org-show-subtree)
        
        ;; Kill the old buffer
        (if org-nanowrimo-setup-editing-buffer-name
            (kill-buffer org-nanowrimo-setup-editing-buffer-name))

        ;; Set us up as the new buffer, need to set the variable first
        ;; so we're recognised correctly.
        (setq org-nanowrimo-setup-editing-buffer-name newbuf-name)
        ;; (message "Am I an editing buffer? %s" (org-nanowrimo-setup-editing-buffer-p))
        (org-nanowrimo-setup-reapply-editing)))


(defun org-nanowrimo-setup-outline-buffer-p ()
  "Check to determine if the current buffer is (probably) the outline buffer."
  (and (eq major-mode 'org-mode)
       (cl-equalp buffer-file-name org-nanowrimo-setup-path)))

(defun org-nanowrimo-setup-editing-buffer-p ()
  "Check to determine if the current buffer is one of the 'org-tree-to-indirect-buffer' editing buffers, or the main editing buffer opened with 'org-nanowrimo-setup-tree-to-editing-buffer'."
    (or (cl-equalp (buffer-name)
             org-nanowrimo-setup-editing-buffer-name)
        
        (and (eql buffer-file-name nil)
             (eq major-mode 'org-mode)
             (string-match (format "%s-" org-nanowrimo-setup-file)
                           (buffer-name)))))

(defun org-nanowrimo-setup-refresh-outline ()
  "Trigger a refresh of the outline mode, probably works fine but may go wrong if you have multiple buffers visiting the same file."
  (interactive)
  (save-excursion
    (save-window-excursion
      (let ((nano-window (get-buffer-window
                          (get-file-buffer org-nanowrimo-setup-path) t)))
        (select-window nano-window)
        (outline-show-all)
        (outline-hide-body)))))

(defun org-nanowrimo-setup-hide-outline ()
  "Deletes the outline window, designed to be used as part of a toggle for hide/showing the outline in combination with org-nanowrimo-setup-reset-window-configuration."
  (if (window-deletable-p org-nanowrimo-setup-outline-window-id)
      (delete-window org-nanowrimo-setup-outline-window-id)))

(defun org-nanowrimo-setup-reset-window-configuration ()
  "Reset the window configuration to how it was when the mode was first established and 'org-nanowrimo-setup-configure-window' ran."
  (if org-nanowrimo-setup-window-configuration
      (set-window-configuration org-nanowrimo-setup-window-configuration)))

(defun org-nanowrimo-setup-outline-window-toggle ()
  "Toggle the outline window in the current window setup.

If the current window list includes the outline then destroy that window using
'org-nanowrimo-setup-hide-outline', if it doesn't exist then restore the
default window configuration using
'org-nanowrimo-setup-reset-window-configuration' which will reshow it, and
also remove any sub windows going on."
  (interactive)
  (if (and (or (org-nanowrimo-setup-outline-buffer-p)
               (org-nanowrimo-setup-editing-buffer-p))
           (seq-contains (window-list) org-nanowrimo-setup-outline-window-id))
      (org-nanowrimo-setup-hide-outline)
    (org-nanowrimo-setup-reset-window-configuration)))

(defun org-nanowrimo-setup-reapply-outline ()
  "Reapply the outline only view for the outline/left hand window.

Relies on 'org-nanowrimo-setup-outline-buffer-p' to determine if in the
correct buffer.

Will also run the 'org-nanowrimo-setup-reapply-outline-buffer-test-function' hook in case the user has any additional checks."
  (if (and (org-nanowrimo-setup-outline-buffer-p)
           (run-hook-with-args-until-failure
            'org-nanowrimo-setup-reapply-outline-buffer-test-function))
      
      (progn
        (outline-hide-body)
        (run-hooks 'org-nanowrimo-setup-reapply-outline-adjustments-hook))))

(defun org-nanowrimo-setup-reapply-editing ()
  "Apply 'visual-line-mode' to the editing buffer/right hand window.
Relies on 'org-nanowrimo-setup-editing-buffer-p' to determine if its in the
right buffer.

Will also run the 'org-nanowrimo-setup-reapply-editing-buffer-test-function'
hook in case the user has any additonal checks."
  (if (and (org-nanowrimo-setup-editing-buffer-p)
           (run-hook-with-args-until-failure
            'org-nanowrimo-setup-reapply-editing-buffer-test-function))
      (progn
        (visual-line-mode 1)
        (run-hooks 'org-nanowrimo-setup-reapply-editing-adjustments-hook))))


(defun org-nanowrimo-setup-export-txt ()
  "Exports the buffer to text, any nodes flagged with the notes filter will get stripped as will all headlines thanks to the other filtering."
  ;; (interactive)
  (if (cl-equalp buffer-file-name org-nanowrimo-setup-path)
      (save-excursion
        (goto-char (point-min))
        (let ((org-export-with-toc nil))
          ;; Filtered elsewhere but here's how you do it otherwise.
          ;; (org-export-exclude-tags '("notes")))

          ;; Actual export occurs here
          (let ((output-file (org-ascii-export-to-ascii nil nil nil t)))

            ;; Word counting functions occur here if wanted
            (if org-nanowrimo-setup-export-count-words

                (with-temp-buffer
                  (insert-file-contents-literally output-file)

                  ;; Using count-words inflates your word count
                  ;; because that relies on forward-word which uses
                  ;; the symbol table, so stops on hypens, dots,
                  ;; quotes, commas, and other stuff.  This is a small
                  ;; count function that matches the output of wc(1)
                  (let ((curr-words (org-nanowrimo-setup-count-words (point-min)
                                                                     (point-max))))
                    (org-nanowrimo-setup-log-wordcount curr-words)
                    (org-nanowrimo-setup-message-wordcount curr-words)))))))))



(defun org-nanowrimo-setup-log-wordcount (curr-words)
  "Takes a count of current words as CURR-WORDS, logs them to a file if 'org-nanowrimo-setup-log-wordcounts' is true.  The file is the same path as the 'org'nanowrimo-setup-path' but with '.stats' as a suffix instead of '.org'."
  (if org-nanowrimo-setup-log-wordcounts
      (append-to-file
       (format "%s word count: %s\n" (format-time-string "%Y-%m-%d %H:%M:%S") curr-words)
       nil
       (format "%s.stats"
               (file-name-sans-extension (expand-file-name org-nanowrimo-setup-path))))))

(defun org-nanowrimo-setup-message-wordcount (curr-words)
  "Takes a count of current words as CURR-WORDS, generate an appropriate message which is then outputted using ' org-nanowrimo-setup-export-message-function'."
  (let ((curr-message (format "Wordcount %d words" curr-words)))
    (cond

     ;; If we're showing how the count is doing
     ;; vs. daily averages then this runs
     ((and org-nanowrimo-setup-export-show-goal
           org-nanowrimo-setup-export-show-moving-goal)
      
      (let* ((remaining (- org-nanowrimo-setup-word-goal
                           curr-words))
             (todays-goal-count (/ remaining
                                   org-nanowrimo-setup-days-remaining)))
        (setq curr-message
              (format "Wordcount %s, %s %s goal %s (Need %s avg | Remaining: %s words, %s days)"
                      curr-words
                      (- curr-words
                         org-nanowrimo-setup-todays-goal)
                      (if (> curr-words
                             org-nanowrimo-setup-todays-goal)
                          "above"
                        "below")
                      org-nanowrimo-setup-todays-goal
                      todays-goal-count
                      remaining
                      org-nanowrimo-setup-days-remaining))))
     
     ;; Just show the goal, don't show the moving
     ;; goals, i.e. the continuing averages.
     (org-nanowrimo-setup-export-show-goal
      (setq curr-message
            (format "Wordcount %s, %s %s goal %s"
                    curr-words
                    (- curr-words
                       org-nanowrimo-setup-todays-goal)
                    (if (> curr-words
                           org-nanowrimo-setup-todays-goal)
                        "above"
                      "below")
                    org-nanowrimo-setup-todays-goal))))
    
    ;; Output whatever message we generated, or just the default if
    ;; nothing extra was wanted.
    (funcall org-nanowrimo-setup-export-message-function
             curr-message)))
                         

(defun org-nanowrimo-setup-output-headings ()
  "Dump the headings and all their tags."
  ;; (interactive)
  (org-map-entries
   (lambda ()
     (let* ((el (org-element-at-point))
            (beg (org-element-property :begin el))
            (end (save-excursion
                   (end-of-line)
                   (point))))
       (progn
         (copy-region-as-kill beg end)
         (message "Header '%s' (%s)" (car kill-ring) org-scanner-tags))))))


(defun org-nanowrimo-setup-export-filtered-tags-removal (backend)
  "Remove all headlines in the current buffer.
BACKEND is the export back-end being used, as a symbol."
  ;; org-cut-subtree *should* work but I don't think it sets
  ;; org-map-continue properly which explains why its not a very
  ;; simple fix.
  ;; 
  ;;(org-map-entries #'org-cut-subtree "+notes")
  
  (org-map-entries
   (lambda ()
     ;; not needed, filter below, however if you wanted to be
     ;; sure, here's definitely one way of doing it - although you
     ;; should probably use org-scanner-tags but *that* will be
     ;; unreliable if you've been hacking around and removing
     ;; chunks of buffer
     ;;
     ;; (if (member "notes" (org-get-tags))
     
     (org-back-to-heading t)
     (let* ((el (org-element-at-point))
            (beg (org-element-property :begin el))
            (end (org-element-property :end el)))
       (progn
         ;; (copy-region-as-kill beg end)
         ;; (message "+notes killed '%s' (%s)" (car kill-ring)
         ;;                                    org-scanner-tags)

         ;; When we delete the region we need to update org to know
         ;; where to continue from, since we have removed everything
         ;; from beg to end then beg is now the "start" of new
         ;; content.  because everything else is nuked
         (delete-region beg end)
         (setq org-map-continue-from beg))))
   org-nanowrimo-setup-export-filtered-tags))


(defun org-nanowrimo-setup-headline-removal (backend)
  "Remove the headlines from the output so its just pure body text.

This is based on the example from the org manual:
https://orgmode.org/manual/Advanced-Export-Configuration.html except that
it actually works because it updates the org mapping to give it a contination
point and instead of just removing them swaps them for \n strings, which
means your body chunks don't get mashed together.

Accept the argument BACKEND which is probably provided by
'org-export-before-parsing-hook'."
  (org-map-entries (lambda ()
                     (let* ((beg (point))
                            (end (line-beginning-position 2)))
                       (progn
                         ;; (copy-region-as-kill beg end)
                         ;; (message "killed-heading '%s' (%s)"
                         ;;          (car kill-ring) org-scanner-tags)
                         (delete-region beg end)
                         (insert org-nanowrimo-setup-export-replace-headlines-str)
                         (setq org-map-continue-from beg))))))

;; FIXME I feel like there's a rounding issue here and it should be
;; 1667 not 1666 fround needed somewhere?
(defun org-nanowrimo-setup-calculate-counts (goal start-date end-date)
  "Calculate the goal word count for the day, set the org-nanowrimo-setup-todays-goal variable as a side effect.  Takes three arguments first GOAL which is the total target, second the START-DATE and END-DATE, which you will want to use 'org-nanowrimo-setup-ymd-to-encode-time' to create."
  (interactive "P")
  (if (and (numberp goal)
           start-date
           end-date)
      (let* ((total-days (- (time-to-days end-date)
                            (time-to-days start-date)))
             (current-day (- (time-to-days (current-time))
                             (time-to-days start-date)))
             (days-remaining (- (time-to-days end-date)
                                (time-to-days (current-time))))
             (daily-goal (/ goal
                            total-days))
             (todays-goal (* daily-goal
                             current-day)))
             (setq org-nanowrimo-setup-todays-goal todays-goal
                   org-nanowrimo-setup-days-remaining days-remaining))))


(defun org-nanowrimo-setup-configure-window (&optional force)
  "Configures the initial frame.

If there is a GUI this maximises the current window.  The frame
is also split vertically with an outline on the left and
contents on the right.  It runs the hook
'org-nanowrimo-setup-theme-adjustments-hook' so that the user can
make manual adjustments to load themes or change fonts or similar.

Adds three functions to Emacs hook list:

A 'buffer-list-update-hook' that calls 'org-nanowrimo-setup-reapply-editing'
that reapplys the desired style/settings to the editing pane on the right.

A 'buffer-list-update-hook' that calls 'org-nanowrimo-setup-reapply-outline'
that reapplies the desired style/settings to the outline pane on the left.

A 'after-save-hook' that calls 'org-nanowrimo-setup-export-txt' to
filter and export the contents of the file enabling you to get a
clean text copy for a wordcount.

When run it will set 'org-nanowrimo-setup-have-configured-frame'
to t and future invocations will returns to run again, you can force
it by providing it with a true FORCE argument or running interactively
and saying 'y'"

  (interactive (list (y-or-n-p "Already applied, do again? ")))
  (if (and (cl-equalp buffer-file-name org-nanowrimo-setup-path)
           (or force
               (not org-nanowrimo-setup-have-configured-frame)))
      (progn
        (message "org-nanowrimo-setup-configure-window: yes (%s)" buffer-file-name)
        (setq org-nanowrimo-setup-have-configured-frame t)

        ;; Start by setting up the goals and timer
        (setq
         org-nanowrimo-setup-goal-update-timer
         (run-with-timer
          0
          600
          (lambda ()
            (org-nanowrimo-setup-calculate-counts org-nanowrimo-setup-word-goal
                                                  org-nanowrimo-setup-start-date
                                                  org-nanowrimo-setup-end-date))))


        ;; Setup our hooks, some of these are expensive/inteferring so
        ;; only load them as appopriate
        ;;
        ;; the export hooks clean up the output, the
        ;; buffer-list-update hooks keep the modes in line for our two
        ;; buffers.
        (if org-nanowrimo-setup-export-filter-headlines
            (add-hook 'org-export-before-parsing-hook
                      'org-nanowrimo-setup-headline-removal))

        (if org-nanowrimo-setup-export-filtered-tags
            (add-hook 'org-export-before-parsing-hook
                      'org-nanowrimo-setup-export-filtered-tags-removal))

        ;; We should be able to hook this of the org-mode-hook but
        ;; org-tree-to-indirect-buffer seems to do something spooky
        ;; that avoids it
        (add-hook 'buffer-list-update-hook 'org-nanowrimo-setup-reapply-editing)
        (add-hook 'buffer-list-update-hook 'org-nanowrimo-setup-reapply-outline)
        (add-hook 'after-save-hook 'org-nanowrimo-setup-export-txt)

        ;; Start by switching over to it.
        (switch-to-buffer org-nanowrimo-setup-file)

        ;; Optionally swap out the theme if wanted, may help focus/shift
        (run-hooks 'org-nanowrimo-setup-theme-adjustments-hook)

        ;; If we're running a window system run any adjustments wanted there
        ;; defaults to toggling into fullscreen mode
        (if (window-system)
            (run-hooks 'org-nanowrimo-setup-gui-adjustments-hook))

        ;; Window config: delete other windows in the frame, split it
        ;; vertically, lock the outline frame if wanted, then open a
        ;; narrowed indirect buffer on the right
        (delete-other-windows)
        (setq org-nanowrimo-setup-outline-window-id (selected-window))
        (let ((fresh-window (split-window-horizontally
                             org-nanowrimo-setup-initial-sidebar-size)))

          (setq org-nanowrimo-setup-editing-window-id fresh-window)

          ;; Try and lock the outline window on the left if wanted
          (if org-nanowrimo-setup-lock-outline-window-size
              (window-preserve-size (window-normalize-window nil t) t t))
          
          ;; We're running in a hook so we don't seem to get our saved
          ;; place before we do the redirect buffer opening so we always
          ;; open at the top of the file.  To avoid this check to see if
          ;; the save-place-mode variable is set and if so then call the
          ;; hook manually, this is kind of messy.
          (if save-place-mode (save-place-find-file-hook))

          ;; Select the new window, then open the current tree as an
          ;; indirect narrowed buffer in there specifically.  If you
          ;; don't do it this way around then it works fine for GUI
          ;; emacs but console emacs will then do *another* split and
          ;; you end up with a 3 column mode.
          (select-window fresh-window)
          (if save-place-mode (save-place-find-file-hook))

          ;; Open in fresh window split
          (org-nanowrimo-setup-tree-to-editing-buffer)

          ;; Old method of opening the fresh split window
          ;; (let ((org-indirect-buffer-display 'current-window))
          ;;   (org-tree-to-indirect-buffer))

          ;; Save the default config
          (setq org-nanowrimo-setup-window-configuration
                (current-window-configuration))))))



;;;###autoload
(define-minor-mode org-nanowrimo-setup-mode
  "When enabled if you open the org-nanowrimo-setup-file then the org-nanowrimo-setup-configure-window function will run and configure your windows and add in a whole bunch of hooks."
  nil
  nil
  `((,(kbd "C-c C-x o") . org-nanowrimo-setup-tree-to-editing-buffer)
    (,(kbd "C-c C-x n") . org-nanowrimo-setup-tree-to-indirect-frame)
    (,(kbd "C-c C-x t") . org-nanowrimo-setup-tree-outline-window-toggle)
    (,(kbd "C-c C-x l") . org-nanowrimo-setup-tree-refresh-outline))
  :global t
  (if org-nanowrimo-setup-mode
      
      (progn
        (message "Enabling nanowrimo-setup-mode")

        ;; (advice-add 'split-window-horizontally :before (lambda (&optional args) (message "Calling split-window-horizontally")))

        (setq org-nanowrimo-setup-have-configured-frame nil)
        
        (if (and (> (length org-nanowrimo-setup-path) 0)
                 (file-exists-p org-nanowrimo-setup-path))
            (progn
              (setq org-nanowrimo-setup-file (file-name-nondirectory org-nanowrimo-setup-path))

              ;; And hook into org-mode
              (add-hook 'org-mode-hook 'org-nanowrimo-setup-configure-window))

          (error (format "Invalid org-nanowrimo-setup-path (%s), unset or non-existant" org-nanowrimo-setup-path))))

    ;; Remove all hooks and everything we setup
    (progn
      (message "Switching off nanowrimo-setup-mode")
      (remove-hook 'org-mode-hook 'org-nanowrimo-setup-configure-window)

      ;; Remove the timer for updating the counts
      (if org-nanowrimo-setup-goal-update-timer
          (cancel-timer org-nanowrimo-setup-goal-update-timer))

      ;; If we've been run then try and remove all the hooks that were setup
      (if org-nanowrimo-setup-have-configured-frame

          (progn
            (if org-nanowrimo-setup-export-filter-headlines
                (remove-hook 'org-export-before-parsing-hook
                             'org-nanowrimo-setup-headline-removal))
            
            (if org-nanowrimo-setup-export-filtered-tags
                (remove-hook 'org-export-before-parsing-hook
                             'org-nanowrimo-setup-export-filtered-tags-removal))
            
            (remove-hook 'buffer-list-update-hook
                         'org-nanowrimo-setup-reapply-editing)
            (remove-hook 'buffer-list-update-hook
                         'org-nanowrimo-setup-reapply-outline)
            (remove-hook 'after-save-hook
                         'org-nanowrimo-setup-export-txt)
            
            ;; Run any user hooks before we're done
            (run-hooks 'org-nanowrimo-setup-switch-off-adjustments)
            
            (setq org-nanowrimo-setup-have-configured-frame nil))))))


(provide 'org-nanowrimo-setup-mode)


;;; org-nanowrimo-setup-mode.el ends here
