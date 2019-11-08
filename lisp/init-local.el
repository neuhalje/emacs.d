;; My custom configuration
;;(setq org-directory (file-name-as-directory (expand-file-name "~/Documents/work/brabbler-ag/org-mode")))
(setq org-directory "~/Documents/work/brabbler-ag/org-mode")
(setq my-org-root org-directory)

;; FIXME: Why does (setq org-agenda-files 'my-gtd-agenda-files) not work?
(setq org-agenda-files (quote (
                               "~/Documents/work/brabbler-ag/org-mode/agenda/gtd.org"
                               "~/Documents/work/brabbler-ag/org-mode/agenda/inbox.org"
                               "~/Documents/work/brabbler-ag/org-mode/agenda/tickler.org"
                               "~/Documents/work/brabbler-ag/org-mode/meeting_notes.org")))
;; setup my agenda files
(setq my-gtd-agenda-files (file-name-as-directory (concat (file-name-as-directory my-org-root) "agenda")))
(setq my-gtd-template-dir (file-name-as-directory (concat (file-name-as-directory my-org-root) "_templates")))


;; define my file names relative to my-org-root
(setq my-gtd-inbox (concat (file-name-as-directory my-gtd-agenda-files) "inbox.org"))
(setq my-gtd-reviews (concat (file-name-as-directory my-org-root) "reviews.org"))
(setq my-gtd-tickler (concat (file-name-as-directory my-gtd-agenda-files) "tickler.org"))
(setq my-gtd-gtd (concat (file-name-as-directory my-gtd-agenda-files) "gtd.org"))
(setq my-gtd-someday (concat (file-name-as-directory my-org-root) "someday.org"))
(setq my-gtd-journal (concat (file-name-as-directory my-org-root) "journal.org"))
(setq my-gtd-meeting_notes (concat (file-name-as-directory my-org-root) "meeting_notes.org"))
(setq my-browser-bookmarks (concat (file-name-as-directory my-org-root) "bookmarking.org"))

(provide 'init-local)
