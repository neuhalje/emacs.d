;;; init-org.el --- Org-mode config -*- lexical-binding: t -*-
;;; Commentary:

;; +mong settings for many aspects of `org-mode', this code includes
;; an opinionated setup for the Getting Things Done (GTD) system based
;; around the Org Agenda.  I have an "inbox.org" file with a header
;; including

;;     #+CATEGORY: Inbox
;;     #+FILETAGS: INBOX

;; and then set this file as `org-default-notes-file'.  Captured org
;; items will then go into this file with the file-level tag, and can
;; be refiled to other locations as necessary.

;; Those other locations are generally other org files, which should
;; be added to `org-agenda-files-list' (along with "inbox.org" org).
;; With that done, there's then an agenda view, accessible via the
;; `org-agenda' command, which gives a convenient overview.
;; `org-todo-keywords' is customised here to provide corresponding
;; TODO states, which should make sense to GTD adherents.

;;; Code:

;(setq org-modules
;  (quote
;   (org-bbdb org-bibtex org-ctags org-docview org-eww org-gnus org-id org-info org-irc org-mhe org-rmail org-w3m org-mac-link org-notmuch org-secretary)))


(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(package-initialize)
(require-package 'org-plus-contrib)

(after-load 'org
(nconc org-modules
       '(
         org-capture
         org-habit
         org-id
         org-protocol
         org-w3m
         org-notmuch
         org-secretary
         ))
)

;; My custom configuration
(setq my-org-root "~/Documents/org-mode/")

;; FIXME: Why does (setq org-agenda-files 'my-gtd-agenda-files) not work?
(setq org-agenda-files (quote (
                               "~/Documents/org-mode/agenda/gtd.org"
                               "~/Documents/org-mode/agenda/inbox.org"
                               "~/Documents/org-mode/agenda/tickler.org"
                               "~/Documents/org-mode/meeting_notes.org")))
;; setup my agenda files
(setq my-gtd-agenda-files (concatenate 'string my-org-root "agenda/"))


;; define my file names relative to my-org-root
(setq my-gtd-inbox (concatenate 'string my-gtd-agenda-files "inbox.org"))
(setq my-gtd-reviews (concatenate 'string my-org-root "reviews.org"))
(setq my-gtd-tickler (concatenate 'string my-gtd-agenda-files "tickler.org"))
(setq my-gtd-gtd (concatenate 'string my-gtd-agenda-files "gtd.org"))
(setq my-gtd-someday (concatenate 'string my-org-root "someday.org"))
(setq my-gtd-journal (concatenate 'string my-org-root "journal.org"))
(setq my-gtd-meeting_notes (concatenate 'string my-org-root "meeting_notes.org"))
(setq my-browser-bookmarks (concatenate 'string my-org-root "bookmarking.org"))

(when *is-a-mac*
  (maybe-require-package 'grab-mac-link))

;; (after-load 'org
;; (when *is-a-mac*
;;     (add-to-list 'org-modules 'org-mac-link t))
;; )

(maybe-require-package 'org-cliplink)

(define-key global-map (kbd "C-c l") 'org-store-link)
(define-key global-map (kbd "C-c a") 'org-agenda)

(defvar sanityinc/org-global-prefix-map (make-sparse-keymap)
  "A keymap for handy global access to org helpers, particularly clocking.")

(define-key sanityinc/org-global-prefix-map (kbd "j") 'org-clock-jump-to-current-clock)
(define-key sanityinc/org-global-prefix-map (kbd "l") 'org-clock-in-last)
(define-key sanityinc/org-global-prefix-map (kbd "i") 'org-clock-in)
(define-key sanityinc/org-global-prefix-map (kbd "o") 'org-clock-out)
(define-key global-map (kbd "C-c o") sanityinc/org-global-prefix-map)


;; Various preferences
;; Record a timestamp when a task is marked as done
;; Log into a separate drawer to keey it clean
(setq org-log-done 'time
      org-log-into-drawer t
      org-edit-timestamp-down-means-later t
      org-hide-emphasis-markers t
      org-catch-invisible-edits 'show
      org-export-coding-system 'utf-8
      org-fast-tag-selection-single-key 'expert
      org-html-validation-link nil
      org-export-kill-product-buffer-when-displayed t
      org-tags-column 80)

;;; org-secretary

;;(require 'org-secretary)
;; Default identity for org-secretary
(setq org-sec-me "jens_neuhalfen")
(defun my/org-sec-inform-with-view (par &optional who)
  "Select tasks marked as inform=who, where who
   defaults to the value of org-sec-with."
  (org-tags-view '(4) (join (split-string (if who
                                              who
                                            (org-sec-get-with)))
                            "|" "inform={" "}")))

;;; org-id
;; https://github.com/tkf/org-mode/blob/master/lisp/org-id.el
;;(require 'org-id)

(setq org-id-link-to-org-use-id t)
(setq org-id-method  'uuid)
(setq org-id-track-globally t)
(setq org-id-locations-file (convert-standard-filename
                             "~/Documents/org-mode/_org-internal/org-id-locations"))

;; Lots of stuff from http://doc.norang.ca/org-mode.html

;; TODO: fail gracefully
(defun sanityinc/grab-ditaa (url jar-name)
  "Download URL and extract JAR-NAME as `org-ditaa-jar-path'."
  ;; TODO: handle errors
  (message "Grabbing %s for org." jar-name)
  (let ((zip-temp (make-temp-name "emacs-ditaa")))
    (unwind-protect
        (progn
          (when (executable-find "unzip")
            (url-copy-file url zip-temp)
            (shell-command (concat "unzip -p " (shell-quote-argument zip-temp)
                                   " " (shell-quote-argument jar-name) " > "
                                   (shell-quote-argument org-ditaa-jar-path)))))
      (when (file-exists-p zip-temp)
        (delete-file zip-temp)))))

(after-load 'ob-ditaa
  (unless (and (boundp 'org-ditaa-jar-path)
               (file-exists-p org-ditaa-jar-path))
    (let ((jar-name "ditaa0_9.jar")
          (url "http://jaist.dl.sourceforge.net/project/ditaa/ditaa/0.9/ditaa0_9.zip"))
      (setq org-ditaa-jar-path (expand-file-name jar-name (file-name-directory user-init-file)))
      (unless (file-exists-p org-ditaa-jar-path)
        (sanityinc/grab-ditaa url jar-name)))))

(after-load 'ob-plantuml
  (let ((jar-name "plantuml.jar")
        (url "http://jaist.dl.sourceforge.net/project/plantuml/plantuml.jar"))
    (setq org-plantuml-jar-path (expand-file-name jar-name (file-name-directory user-init-file)))
    (unless (file-exists-p org-plantuml-jar-path)
      (url-copy-file url org-plantuml-jar-path))))


;; Re-align tags when window shape changes
(after-load 'org-agenda
  (add-hook 'org-agenda-mode-hook
            (lambda () (add-hook 'window-configuration-change-hook 'org-agenda-align-tags nil t))))




(maybe-require-package 'writeroom-mode)

(define-minor-mode prose-mode
  "Set up a buffer for prose editing.
This enables or modifies a number of settings so that the
experience of editing prose is a little more like that of a
typical word processor."
  nil " Prose" nil
  (if prose-mode
      (progn
        (when (fboundp 'writeroom-mode)
          (writeroom-mode 1))
        (setq truncate-lines nil)
        (setq word-wrap t)
        (setq cursor-type 'bar)
        (when (eq major-mode 'org)
          (kill-local-variable 'buffer-face-mode-face))
        (buffer-face-mode 1)
        ;;(delete-selection-mode 1)
        (setq-local blink-cursor-interval 0.6)
        (setq-local show-trailing-whitespace nil)
        (setq-local line-spacing 0.2)
        (setq-local electric-pair-mode nil)
        (ignore-errors (flyspell-mode 1))
        (visual-line-mode 1))
    (kill-local-variable 'truncate-lines)
    (kill-local-variable 'word-wrap)
    (kill-local-variable 'cursor-type)
    (kill-local-variable 'blink-cursor-interval)
    (kill-local-variable 'show-trailing-whitespace)
    (kill-local-variable 'line-spacing)
    (kill-local-variable 'electric-pair-mode)
    (buffer-face-mode -1)
    ;; (delete-selection-mode -1)
    (flyspell-mode -1)
    (visual-line-mode -1)
    (when (fboundp 'writeroom-mode)
      (writeroom-mode 0))))

;;(add-hook 'org-mode-hook 'buffer-face-mode)

(setq org-support-shift-select t)

;;; Capturing

(global-set-key (kbd "C-c c") 'org-capture)

(setq org-capture-templates
      '(
        ;;  TASK for someone else
	("t" "TODO")
        ;; TASK for someone else
        ("tf" "TASK      (f) Task for someone else" entry (file my-gtd-inbox)
         "* TASK %? :%^G:
:PROPERTIES:
:dowith:
:inform:
:END:
:LOGBOOK:
- State \"TASK\"       from \"\"           %U
:END:" :empty-lines 1)
        ;; TODO     (t) Todo template
        ("tt" "TODO      (t) Todo" entry (file my-gtd-inbox)
         "* TODO %?
:PROPERTIES:
:dowith:
:inform:
:END:
:LOGBOOK:
- State \"TODO\"       from \"\"           %U
:END:" :empty-lines 1)

        ;; WAITING  (w) Waiting template
        ("tw" "WAITING   (w) Waiting" entry (file my-gtd-inbox)
         "* WAITING %? :%^G:
:PROPERTIES:
:dowith:
:inform:
:END:
:LOGBOOK:
- State \"WAITING\"    from \"\"           %U
:END:" :empty-lines 1)
        ;; CANCELLED(x) Cancelled template
        ("tx" "CANCELLED (x) Cancelled" entry (file my-gtd-inbox)
         "* CANCELLED %?
:PROPERTIES:
:inform:
:END:
:LOGBOOK:
- State \"CANCELLED\"       from \"\"           %U
:END:" :empty-lines 1)

        ;; DONE     (d) Done template
        ("td" "DONE      (d) Done" entry (file my-gtd-inbox)
         "* DONE %?
:PROPERTIES:
:inform:
:END:
:LOGBOOK:
- State \"DONE\"       from \"\"           %U
:END:" :empty-lines 1)

        ("j" "Journal" entry (file+datetree my-gtd-journal)
         "* %?\nEntered on %U\n  %i")

	("n" "Notes")
        ("nn" "Note" entry (file my-gtd-inbox)
         "* NOTE %?\nEntered on %U\n  %i
:PROPERTIES:
:inform:
:END:
")
        ("nm" "generic meeting notes" entry (file my-gtd-inbox)
         (file "~/Documents/org-mode/_templates/meeting_note.org"))
        ("n1" "1:1 meeting" entry (file my-gtd-inbox)
         (file "~/Documents/org-mode/_templates/meeting_note_1on1.org"))

	("r" "Reviews")
        ("rp" "Review: daily planing" entry (file+datetree my-gtd-reviews)
         (file "~/Documents/org-mode/_templates/morning_review.org"))
        ("rd" "Review: Daily Afternoon Review" entry (file+datetree my-gtd-reviews)
         (file "~/Documents/org-mode/_templates/dailyreview.org"))
        ("rw" "Review: Weekly Review" entry (file+datetree my-gtd-reviews)
         (file "~/Documents/org-mode/_templates/weeklyreview.org"))

        ("T" "Tickler" entry (file+headline my-gtd-tickler "Tickler")
         "* %i%? \n %U")

	("c" "org-protocol capturing")
           ("cp" "Protocol" entry (file my-gtd-inbox)
                    "* [[%:link][%^{Title}]]
:PROPERTIES:
:CREATED: %U
:SOURCE:  %:link
:TITLE:   %:description
:END:

#+BEGIN_QUOTE
%i
#+END_QUOTE

%?")
            ("cl" "Protocol Link" entry (file my-gtd-inbox)
                    "* [[%:link][%^{Title}]]
:PROPERTIES:
:CREATED: %U
:SOURCE:  %:link
:TITLE:   %:description
:END:

%?")
        ))

;;; Refiling

(setq org-refile-use-cache nil)

(setq org-refile-targets '(
                           (my-gtd-gtd :tag . "prj")
                           (my-gtd-gtd :tag . "refile_target")
                           (my-gtd-meeting_notes :level . 2)
                           (my-gtd-someday :level . 1)
                           (my-gtd-tickler :maxlevel . 2)
                           (my-browser-bookmarks :tag . "refile_target")
                           ))

(after-load 'org-agenda
  (add-to-list 'org-agenda-after-show-hook 'org-show-entry))

(advice-add 'org-refile :after (lambda (&rest _) (org-save-all-org-buffers)))

;; Exclude DONE state tasks from refile targets
(defun sanityinc/verify-refile-target ()
  "Exclude todo keywords with a done state from refile targets."
  (not (member (nth 2 (org-heading-components)) org-done-keywords)))
(setq org-refile-target-verify-function 'sanityinc/verify-refile-target)

(defun sanityinc/org-refile-anywhere (&optional goto default-buffer rfloc msg)
  "A version of `org-refile' which allows refiling to any subtree."
  (interactive "P")
  (let ((org-refile-target-verify-function))
    (org-refile goto default-buffer rfloc msg)))

(defun sanityinc/org-agenda-refile-anywhere (&optional goto rfloc no-update)
  "A version of `org-agenda-refile' which allows refiling to any subtree."
  (interactive "P")
  (let ((org-refile-target-verify-function))
    (org-agenda-refile goto rfloc no-update)))

;; Targets start with the file name - allows creating level 1 tasks
;;(setq org-refile-use-outline-path (quote file))
(setq org-refile-use-outline-path t)
(setq org-outline-path-complete-in-steps nil)

;; Allow refile to create parent tasks with confirmation
(setq org-refile-allow-creating-parent-nodes 'confirm)


;;; To-do settings

(setq org-todo-keyword-faces
      (quote (("NEXT" :inherit warning)
              ("prj" :inherit font-lock-string-face))))

;; https://orgmode.org/manual/Tracking-TODO-state-changes.html#Tracking-TODO-state-changes
;; "!" means timestamp
;; "@" means note
;;   I keep todo items for my team members and for me. I encode mine with TODO,
;;   the team's with TASK:
(setq org-todo-keywords
      '(
        ;; Tasks for me
        (sequence "TODO(t)" "NEXT(x)" "STARTED(s!)" "WAITING(w@/!)"
                  "|" "DONE(d!)" "CANCELED(c@)")
        ;; Tasks for others
        (sequence "TASK(f@/!)"  "|" "DONE(d!)" "CANCELED(c@)")
        ;; Meetings
        (sequence "MEETING" "|" "MEETING_PROTOCOL" "NOTE")
        ;; org-protocol
        (sequence "LINK_TO_SORT" "|" "LINK")
        ))


;;; Agenda views

(setq-default org-agenda-clockreport-parameter-plist '(:link t :maxlevel 3))

;; Aktuelle Zeile in der Agenda hervorheben
(add-hook 'org-agenda-mode-hook '(lambda () (hl-line-mode 1 )))


;; Don not show tasks tagged as SOEMDAY in the agenda
(setq org-agenda-filter-preset '("-someday"))

(setq org-agenda-format-date
      "%Y-%m-%d ---------------------------------------------------------------------")

;; Complex activities are projects, and are marked with the prj tag. They are
;; supposed to contain todo items, and are considered stuck unless they do.
;; The prj tag should not be inheritable, because otherwise its tasks will
;; appear as projects:

(setq org-tags-exclude-from-inheritance '("prj" "refile_target")
      org-stuck-projects '("+prj/-MAYBE-DONE"
                           ("TODO" "TASK" "NEXT" "WAITING") ()))

(setq org-todo-keyword-faces
      '(("TODO" . (:foreground "DarkOrange1" :weight bold))
        ("MAYBE" . (:foreground "sea green"))
        ("DONE" . (:foreground "light sea green"))
        ("CANCELLED" . (:foreground "forest green"))
        ("TASK" . (:foreground "light blue"))
        ("MEETING" . (:foreground "gray"))
        ("NOTE" . (:foreground "gray"))
        ))


;; Skip "done" tasks in agenda, even when schedules/deadline
;; Use log mode to view them
(setq org-agenda-skip-deadline-if-done t)
(setq org-agenda-skip-scheduled-if-done t)

;; show the diary in the agenda
(setq org-agenda-include-diary t)

  (setq org-agenda-compact-blocks t
        org-agenda-sticky t
        org-agenda-start-on-weekday nil
        org-agenda-span 'day
        org-agenda-include-diary nil
        org-agenda-sorting-strategy
        '((agenda habit-down time-up user-defined-up effort-up category-keep)
          (todo category-up effort-up)
          (tags category-up effort-up)
          (search category-up))
        org-agenda-window-setup 'current-window)


(setq org-agenda-custom-commands
      (quote (
              ("A" "Agenda w. NEXT, WAITING, TASK"
               ((todo "WAITING")
                (todo "TASK")
                (todo "NEXT")
                (tags "flagged")
                (agenda "")))

              ("M" "Meeting with *C-c w*"  ((my/org-sec-inform-with-view "inform")
                                            (org-sec-with-view "TODO dowith")
                                            (org-sec-where-view "TODO doat")
                                            (org-sec-assigned-with-view "TASK with")
                                            (org-sec-stuck-with-view "STUCK with")))

              ("o" .  "Other agendas")
              ;;              ("gn" "Next Actions" todo "NEXT" ((org-use-tag-inheritance nil)))
              ("od" "DONE" todo "DONE" ((org-use-tag-inheritance nil)))
              ;;              ("gw" "Waiting" todo "WAITING")
              ;;              ("gw" "Waiting" todo "WAITING")

              ;;              ("r" . "Review Workflow")

              ("os" "Stuck projects" ((org-ql-block '(and (not (todo)) (tags "prj") (not (done)) (not (descendants (todo "NEXT")))) ((org-ql-block-header "Stuck Projects")))))
              ("oS" "Stuck projects2" ((org-ql-block '(not (or ((and (todo "DONE") (descendants (todo "DONE")) (descendants "NEXT"))))))))

              ;;            ("@" . "Location based")
              ;;          ("@o" "At the office" tags-todo "@office" ((org-agenda-overriding-header "Office")))
              ;;        ("@h" "At home" tags-todo "@home" ((org-agenda-overriding-header "Home")))

              ("s" . "Secretary")
              ("sh" "Work todos" tags-todo
               "-personal-doat={.+}-dowith={.+}/!-TASK"
               ((org-agenda-todo-ignore-scheduled t)))
              ("sH" "All work todos" tags-todo "-personal/!-TASK-MAYBE"
               ((org-agenda-todo-ignore-scheduled nil)))
              ("sA" "Work todos with doat or dowith" tags-todo
               "-personal+doat={.+}|dowith={.+}/!-TASK"
               ((org-agenda-todo-ignore-scheduled nil)))
              ("sj" "inform, TODO dowith and TASK with"
               ((my/org-sec-inform-with-view "inform")
                (org-sec-with-view "TODO dowith")
                (org-sec-where-view "TODO doat")
                (org-sec-assigned-with-view "TASK with")
                (org-sec-stuck-with-view "STUCK with")))
              ("sJ" "Interactive TODO dowith and TASK with"
               ((org-sec-who-view "TODO dowith")))
              )))

;;
;;        org-agenda-custom-commands
;;        `(("N" "Notes" tags "NOTE"
;;           ((org-agenda-overriding-header "Notes")
;;            (org-tags-match-list-sublevels t)))
;;          ("g" "GTD"
;;           ((agenda "" nil)
;;            (tags "INBOX"
;;                  ((org-agenda-overriding-header "Inbox")
;;                   (org-tags-match-list-sublevels nil)))
;;            (stuck ""
;;                   ((org-agenda-overriding-header "Stuck Projects")
;;                    (org-agenda-tags-todo-honor-ignore-options t)
;;                    (org-tags-match-list-sublevels t)
;;                    (org-agenda-todo-ignore-scheduled 'future)))
;;            (tags-todo "-INBOX"
;;                       ((org-agenda-overriding-header "Next Actions")
;;                        (org-agenda-tags-todo-honor-ignore-options t)
;;                        (org-agenda-todo-ignore-scheduled 'future)
;;                        (org-agenda-skip-function
;;                         '(lambda ()
;;                            (or (org-agenda-skip-subtree-if 'todo '("HOLD" "WAITING"))
;;                                (org-agenda-skip-entry-if 'nottodo '("NEXT")))))
;;                        (org-tags-match-list-sublevels t)
;;                        (org-agenda-sorting-strategy
;;                         '(todo-state-down effort-up category-keep))))
;;            (tags-todo ,active-project-match
;;                       ((org-agenda-overriding-header "Projects")
;;                        (org-tags-match-list-sublevels t)
;;                        (org-agenda-sorting-strategy
;;                         '(category-keep))))
;;            (tags-todo "-INBOX/-NEXT"
;;                       ((org-agenda-overriding-header "Orphaned Tasks")
;;                        (org-agenda-tags-todo-honor-ignore-options t)
;;                        (org-agenda-todo-ignore-scheduled 'future)
;;                        (org-agenda-skip-function
;;                         '(lambda ()
;;                            (or (org-agenda-skip-subtree-if 'todo '("PROJECT" "HOLD" "WAITING" "DELEGATED"))
;;                                (org-agenda-skip-subtree-if 'nottododo '("TODO")))))
;;                        (org-tags-match-list-sublevels t)
;;                        (org-agenda-sorting-strategy
;;                         '(category-keep))))
;;            (tags-todo "/WAITING"
;;                       ((org-agenda-overriding-header "Waiting")
;;                        (org-agenda-tags-todo-honor-ignore-options t)
;;                        (org-agenda-todo-ignore-scheduled 'future)
;;                        (org-agenda-sorting-strategy
;;                         '(category-keep))))
;;            (tags-todo "/DELEGATED"
;;                       ((org-agenda-overriding-header "Delegated")
;;                        (org-agenda-tags-todo-honor-ignore-options t)
;;                        (org-agenda-todo-ignore-scheduled 'future)
;;                        (org-agenda-sorting-strategy
;;                         '(category-keep))))
;;            (tags-todo "-INBOX"
;;                       ((org-agenda-overriding-header "On Hold")
;;                        (org-agenda-skip-function
;;                         '(lambda ()
;;                            (or (org-agenda-skip-subtree-if 'todo '("WAITING"))
;;                                (org-agenda-skip-entry-if 'nottodo '("HOLD")))))
;;                        (org-tags-match-list-sublevels nil)
;;                        (org-agenda-sorting-strategy
;;                         '(category-keep))))
;;            ;; (tags-todo "-NEXT"
;;            ;;            ((org-agenda-overriding-header "All other TODOs")
;;            ;;             (org-match-list-sublevels t)))
;;            )))))


(add-hook 'org-agenda-mode-hook 'hl-line-mode)


;;; Org clock

;; Save the running clock and all clock history when exiting Emacs, load it on startup
(after-load 'org
  (org-clock-persistence-insinuate))
(setq org-clock-persist t)
(setq org-clock-in-resume t)

;; Save clock data and notes in the LOGBOOK drawer
(setq org-clock-into-drawer t)
;; Save state changes in the LOGBOOK drawer
(setq org-log-into-drawer t)
;; Removes clocked tasks with 0:00 duration
(setq org-clock-out-remove-zero-time-clocks t)

;; Show clock sums as hours and minutes, not "n days" etc.
(setq org-time-clocksum-format
      '(:hours "%d" :require-hours t :minutes ":%02d" :require-minutes t))



;;; Show the clocked-in task - if any - in the header line
(defun sanityinc/show-org-clock-in-header-line ()
  (setq-default header-line-format '((" " org-mode-line-string " "))))

(defun sanityinc/hide-org-clock-from-header-line ()
  (setq-default header-line-format nil))

(add-hook 'org-clock-in-hook 'sanityinc/show-org-clock-in-header-line)
(add-hook 'org-clock-out-hook 'sanityinc/hide-org-clock-from-header-line)
(add-hook 'org-clock-cancel-hook 'sanityinc/hide-org-clock-from-header-line)

(after-load 'org-clock
  (define-key org-clock-mode-line-map [header-line mouse-2] 'org-clock-goto)
  (define-key org-clock-mode-line-map [header-line mouse-1] 'org-clock-menu))



(when (and *is-a-mac* (file-directory-p "/Applications/org-clock-statusbar.app"))
  (add-hook 'org-clock-in-hook
            (lambda () (call-process "/usr/bin/osascript" nil 0 nil "-e"
                                (concat "tell application \"org-clock-statusbar\" to clock in \"" org-clock-current-task "\""))))
  (add-hook 'org-clock-out-hook
            (lambda () (call-process "/usr/bin/osascript" nil 0 nil "-e"
                                "tell application \"org-clock-statusbar\" to clock out"))))



;; TODO: warn about inconsistent items, e.g. TODO inside non-PROJECT
;; TODO: nested projects!



;;; Archiving

(setq org-archive-mark-done nil)
(setq org-archive-location "%s_archive::* Archive")

;; Confgure org-attach

;; Use symbolic links to attach files
(setq org-attach-method (quote lns))

(require-package 'org-pomodoro)
(setq org-pomodoro-keep-killed-pomodoro-time t)
(after-load 'org-agenda
  (define-key org-agenda-mode-map (kbd "P") 'org-pomodoro))


;; ;; Show iCal calendars in the org agenda
;; (when (and *is-a-mac* (require 'org-mac-iCal nil t))
;;   (setq org-agenda-include-diary t
;;         org-agenda-custom-commands
;;         '(("I" "Import diary from iCal" agenda ""
;;            ((org-agenda-mode-hook #'org-mac-iCal)))))

;;   (add-hook 'org-agenda-cleanup-fancy-diary-hook
;;             (lambda ()
;;               (goto-char (point-min))
;;               (save-excursion
;;                 (while (re-search-forward "^[a-z]" nil t)
;;                   (goto-char (match-beginning 0))
;;                   (insert "0:00-24:00 ")))
;;               (while (re-search-forward "^ [a-z]" nil t)
;;                 (goto-char (match-beginning 0))
;;                 (save-excursion
;;                   (re-search-backward "^[0-9]+:[0-9]+-[0-9]+:[0-9]+ " nil t))
;;                 (insert (match-string 0))))))


(after-load 'org
  (define-key org-mode-map (kbd "C-M-<up>") 'org-up-element)
  (when *is-a-mac*
    (define-key org-mode-map (kbd "M-h") nil)
    (define-key org-mode-map (kbd "C-c g") 'org-mac-grab-link)))

(after-load 'org
  (org-babel-do-load-languages
   'org-babel-load-languages
   `((R . t)
     (ditaa . t)
     (dot . t)
     (emacs-lisp . t)
     (gnuplot . t)
     (haskell . nil)
     (latex . t)
     (ledger . t)
     (ocaml . nil)
     (octave . t)
     (plantuml . t)
     (python . t)
     (ruby . t)
     (screen . nil)
     (,(if (locate-library "ob-sh") 'sh 'shell) . t)
     (sql . t)
     (sqlite . t))))


(provide 'init-org)
;;; init-org.el ends here
