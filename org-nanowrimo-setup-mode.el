;;; org-nanowrimo-setup-mode.el --- A basic mode to setup a frame for nanorimo editing, outline left side, editing right side, accurate wordcount on plain text export

;; Copyright 2020 - Twitchy Ears

;; Author: Twitchy Ears https://github.com/twitchy-ears/
;; URL: https://github.com/twitchy-ears/org-nanowrimo-setup-mode
;; Version: 0.1
;; Package-Requires ((emacs "26") (org "9"))
;; Keywords: org nanowrimo

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

;; Essentially all you need is this:
;; (use-package org)
;; (use-package org-nanowrimo-setup-mode
;;     :after org
;;     :init
;;     (setq org-nanowrimo-setup-path (expand-file-name "/path/to/story-file.org"))
;;     :config
;;     (org-nanowrimo-setup-mode))
;;
;;
;;
;; A longer example includes:
;; (use-package org)
;; (use-package nanowrimo-setup-mode
;;   :after org
;;   :init
;;   (setq org-nanowrimo-setup-path (expand-file-name
;;                            "~/nano2020/nano.org"))
;; 
;;   ;; Outline (left hand pane) tests/adjustments
;;   (add-hook 'org-nanowrimo-setup-reapply-outline-adjustments-hook
;;             (lambda ()
;;               (set-face-background 'hl-line "grey10")
;;               (set-face-attribute 'isearch nil :background "DarkGreen")))
;; 
;;   ;; Editing (right hand pane) tests and adjustments
;;   (add-hook 'org-nanowrimo-setup-reapply-editing-buffer-test-function
;;             (lambda () (not darkroom-mode)))
;; 
;;   (add-hook 'org-nanowrimo-setup-reapply-editing-adjustments-hook
;;             (lambda ()
;;               (progn
;;                 (company-mode -1)
;;                 (make-local-variable 'scroll-margin)
;;                 (setq scroll-margin 5)
;;                 (if (not darkroom-mode)
;;                     (darkroom-mode)))))
;; 
;;   ;; Theme tweaks/changes
;;   (add-hook 'org-nanowrimo-setup-theme-adjustments-hook
;;             (lambda ()
;;               (progn
;;                 (load-theme 'farmhouse-dark)
;;                 (set-face-attribute 'hl-line nil
;;                                     :inherit nil
;;                                     :background "gray10")
;;                 (company-mode -1))))
;;   
;;   :config
;;   (if (file-exists-p org-nanowrimo-setup-path)
;;       (progn
;;         (define-key org-mode-map (kbd "C-c C-x n")
;;           'org-nanowrimo-setup-tree-to-indirect-frame)
;;         (use-package darkroom
;;           :ensure t
;;           :init (setq darkroom-text-scale-increase 0.6))
;;         (use-package "farmhouse-theme"
;;           :ensure t
;;           :defer t)
;;         (org-nanowrimo-setup-mode))))
;;
;;
;; It should be noted that this will take over the frame its in and
;; cause some havoc in its wake (fullscreening stuff creating splits,
;; and optionally changing themes around) it will also export a plain
;; text version to "~/nano2020/nano.txt" every time you write a
;; change to the .org file, this allows you to have accurate wordcount
;; without any weird forms of filtering the current buffer

;; TODO: This should probably be looking at org-sidebar to see if
;; that would do anything more useful for me:
;; https://github.com/alphapapa/org-sidebar
;;
;; Also maybe transposing stuff instead of opening indirect buffers 

(require 'org)
(require 'ox)
(require 'ox-ascii)
(require 'org-element)

(defvar org-nanowrimo-setup-path nil "Path to the .org file where your nanowrimo project will be stored.  This should point to an existing file to trigger the creation of the org-mode-hook that sets this all up")

(defvar org-nanowrimo-setup-have-configured-frame nil "Is set to t once a frame has been configured with the org-nanowrimo-setup-configure-window function, this reduces frame mangling on revisiting")

(defvar org-nanowrimo-setup-initial-sidebar-size 40 "Size of the lefthand sidebar")
(defvar org-nanowrimo-setup-lock-outline-window-size t "Fix the outline windows width")

(defvar org-nanowrimo-setup-export-filtered-tags "+notes" "Org tag selection string for tagged trees to filter out of the export, see: https://orgmode.org/manual/Matching-tags-and-properties.html")

(defvar org-nanowrimo-setup-export-filter-headlines t "If headlines should be filtered out of the export")

(defvar org-nanowrimo-setup-export-replace-headlines-str "" "String to replace headlines with")

(defvar org-nanowrimo-setup-theme-adjustments-hook nil "Hook to run before checking for GUI to make any theme tweaks/changes you want")

(defvar org-nanowrimo-setup-gui-adjustments-hook
  nil
  "Hook that runs if org-nanowrimo-setup-configure-window is inside a GUI as tested by (window-system).  Alternative ideas include just resizing the frame:
(lambda () (set-frame-size (selected-frame) 160 45))")
(add-hook 'org-nanowrimo-setup-gui-adjustments-hook #'toggle-frame-fullscreen)

(defvar org-nanowrimo-setup-reapply-editing-buffer-test-function nil "Hook that runs when testing to see if org-nanowrimo-setup-reapply-editing should run, useful to test to see if a mode is already enabled if you want to call a toggle.  Every hooked function must return t")

(defvar org-nanowrimo-setup-reapply-editing-adjustments-hook nil "Additional code to run when adjusting the editing right hand frame, this will run whenever that frame is entered due to the hook, so be careful.")

(defvar org-nanowrimo-setup-reapply-outline-buffer-test-function nil "Hook that runs when testing to see if org-nanowrimo-setup-reapply-outline should be run, useful to test to see if a mode is already enabled if you want to call a toggle.  Every hooked function must return t")

(defvar org-nanowrimo-setup-reapply-outline-adjustments-hook nil "Hook that runs when adjusting the outline left hand frame, this will run whenever that frame is entered due to the hook, so be careful.")

(defvar org-nanowrimo-setup-switch-off-adjustments-hook nil "Additional code to run when switching the mode off if it has been enabled, so reverting themes and so forth.  Expects a lambda.")

(defvar org-nanowrimo-setup-export-count-words t "Show words after export")
(defvar org-nanowrimo-setup-export-show-goal t "Calculate daily goals and show how export is doing for under/over shooting this")
(defvar org-nanowrimo-setup-goal-update-timer nil "Holds the timer that is used to refresh the word count")


;; See also (org-time-convert-to-integer
;;              (my/ymd-to-encode-time "2020-11-01"))
(defun my/ymd-to-encode-time (ymd)
  "Expects a string in the form YYYY-MM-DD and returns an emacs time string structure in response which can then be fed into format-time-string or time-to-days or other functions"
  (let ((p (parse-time-string ymd)))
    (encode-time 0 0 0 (nth 3 p) (nth 4 p) (nth 5 p))))

(defvar org-nanowrimo-setup-start-date
  (my/ymd-to-encode-time (format-time-string "%Y-11-00" (current-time)))
  "Start date of the writing challenge, defaults to November of the current year")

(defvar org-nanowrimo-setup-end-date
  (my/ymd-to-encode-time (format-time-string "%Y-11-30" (current-time)))
  "End date of the writing challenge, defaults to 30th November of the current year")

(defvar org-nanowrimo-setup-word-goal 50000 "Number of words you're aiming to hit between org-nanowrimo-setup-start-date and org-nanowrimo-setup-end-date")


(defun my/org-get-tags ()
  "Dump out the tags at point as a message in the minibuffer, essentially just a wrapper with (interactive)"
  (interactive)
  (message "%s" (org-get-tags)))

;; Borrowed from https://www.emacswiki.org/emacs/WordCount and attempt
;; to include hypenated words and other stuff.
;;(defun org-nanowrimo-setup-count-words (start end &optional allbuffer)
;;    "Print number of words in the region."
;;    (interactive "r")
;;    (save-excursion
;;      (save-restriction
;;        (progn
;;          (if allbuffer
;;              (narrow-to-region (point-min) (point-max))
;;            (narrow-to-region start end))
;;          (goto-char (point-min))
;;
;;          ;; Allow words to include hypen and dot and others
;;          (modify-syntax-entry ?- "w")
;;          (modify-syntax-entry ?\. "w")
;;          (modify-syntax-entry ?\" "w")
;;          (modify-syntax-entry ?' "w")
;;          (modify-syntax-entry ?, "w")
;;          (modify-syntax-entry ?? "w")
;;         
;;          (let ((wcount (count-matches "\\sw+")))
;;            (if (called-interactively-p 'any)
;;                (progn
;;                  (message "Words: %d" wcount)
;;                  wcount)
;;              wcount))))))

(defun org-nanowrimo-setup-count-words (start end)
  "Inspired by simple.el version of count-words but also matches the count used by wc(1) because its counting whitespaces rather than forward-word or forward-symbol.  This will take a start and end for a region when called from elisp or when called interactively expects a region.  Returns the count but outputs a message as well if called interactively."
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
  "Wraps org-tree-to-indirect-buffer while setting org-indirect-buffer-display to 'new-frame' so it opens in a fresh frame"
  (interactive)
  (let ((org-indirect-buffer-display 'new-frame))
    (org-tree-to-indirect-buffer ARG)))

(defun org-nanowrimo-setup-configure-window (&optional force)
  "Configures the initial frame.  By default if there is a GUI this maximises it, regardless it splits it vertically with an outline on the left and contents on the right which has been put into darkroom mode.  Also sets up an after-save-hook that filters and exports the contents of the file enabling you to get a clean text copy for a wordcount.

If already run will set org-nanowrimo-setup-have-configured-frame and refuse to run again, you can force it by providing it with a true argument or running interactively and saying 'y'"
  (interactive (list (y-or-n-p "Already applied, do again? ")))
  (if (and (cl-equalp buffer-file-name org-nanowrimo-setup-path)
           (or force
               (not org-nanowrimo-setup-have-configured-frame)))
      (progn 
        (message "org-nanowrimo-setup-configure-window: yes (%s)" buffer-file-name)
        (setq org-nanowrimo-setup-have-configured-frame t)

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
        (let ((fresh-window (split-window-horizontally
                             org-nanowrimo-setup-initial-sidebar-size)))

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
          (let ((org-indirect-buffer-display 'current-window))
            (org-tree-to-indirect-buffer))))))

(defun org-nanowrimo-setup-reapply-outline ()
  "Reapplies the outline only view for the outline/left hand window"
  (if (and (eq major-mode 'org-mode)
           (cl-equalp buffer-file-name org-nanowrimo-setup-path)
           (run-hook-with-args-until-failure
            'org-nanowrimo-setup-reapply-outline-buffer-test-function))
      
      (progn
        (outline-hide-body)
        (run-hooks 'org-nanowrimo-setup-reapply-outline-adjustments-hook))))

(defun org-nanowrimo-setup-reapply-editing ()
  "Apply visual-line-mode to the editing buffer/right hand window"
  (if (and (eql buffer-file-name nil)
           (eq major-mode 'org-mode)
           (string-match (format "%s-" org-nanowrimo-setup-file) (buffer-name))
           (run-hook-with-args-until-failure
            'org-nanowrimo-setup-reapply-editing-buffer-test-function))
      (progn
        (visual-line-mode 1)
        (run-hooks 'org-nanowrimo-setup-reapply-editing-adjustments-hook))))

(defun org-nanowrimo-setup-export-txt ()
  "Exports the buffer to text, any nodes flagged with the notes filter will get stripped as will all headlines thanks to the other filtering"
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
                  ;; (let ((curr-words (count-words (point-min) (point-max))))

                  ;; Using count-words inflates your word count
                  ;; because that relies on forward-word which uses
                  ;; the symbol table, so stops on hypens, dots,
                  ;; quotes, commas, and other stuff.  This is a small
                  ;; count function that matches the output of wc(1)
                  (let ((curr-words (org-nanowrimo-setup-count-words (point-min)
                                                                     (point-max))))

                    ;; If we're showing how the count is doing
                    ;; vs. daily averages then this runs
                    (if org-nanowrimo-setup-export-show-goal
                        (message "Export (%s) has %s words, %s %s goal (%s)"
                                 output-file
                                 curr-words
                                 (- curr-words org-nanowrimo-setup-todays-goal)
                                 (if (> curr-words org-nanowrimo-setup-todays-goal)
                                     "above"
                                   "below")
                                 org-nanowrimo-setup-todays-goal)

                      ;; Otherwise just show the regular count
                      (message "Export (%s) has %s words"
                               output-file
                               curr-words))))))))))


(defun org-nanowrimo-setup-output-headings ()
  "Dump the headings and all their tags"
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
  "Remove the headlines from the output so its just pure body text.  This is based on the example from the org manual https://orgmode.org/manual/Advanced-Export-Configuration.html except that it actually works because it updates the org mapping to give it a contination point and instead of just removing them swaps them for \n strings, which means your body chunks don't get mashed together."
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

(defun org-nanowrimo-setup-calculate-counts (goal start-date end-date)
  "Calculate the goal word count for the day, set the org-nanowrimo-setup-todays-goal variable as a side effect"
  (interactive)
  (if (and (numberp goal)
           start-date
           end-date)
      (let* ((total-days (- (time-to-days end-date)
                            (time-to-days start-date)))
             (current-day (- (time-to-days (current-time))
                             (time-to-days start-date)))
             (daily-goal (/ goal
                            total-days))
             (todays-goal (* daily-goal
                             current-day)))
        (setq org-nanowrimo-setup-todays-goal todays-goal))))


(define-minor-mode org-nanowrimo-setup-mode
  "When enabled if you open the org-nanowrimo-setup-file then the org-nanowrimo-setup-configure-window function will run and configure your windows and add in a whole bunch of hooks."
  nil
  nil
  nil
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
              ;; Setup the goal counter
              (setq
               org-nanowrimo-setup-goal-update-timer
               (run-with-timer
                0
                600
                (lambda ()       
                  (org-nanowrimo-setup-calculate-counts org-nanowrimo-setup-word-goal
                                                        org-nanowrimo-setup-start-date
                                                        org-nanowrimo-setup-end-date))))
              
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


