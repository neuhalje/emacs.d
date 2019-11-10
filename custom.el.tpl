;;; custom.el --- Local settings and changes by 'customize'

;;; Commentary:
;;;
;;; These are the "minimum" local configurations needed for this setup
;;; to work.

;;; Code:

;;;; Values set by customize-*

(custom-set-variables
;; custom-set-variables was added by Custom.
;; If you edit it by hand, you could mess it up, so be careful.
;; Your init file should contain only one such instance.
;; If there is more than one, they won't work right.
'(custom-enabled-themes (quote (sanityinc-tomorrow-bright)))
'(mail-envelope-from nil)
'(message-sendmail-envelope-from nil)
'(notmuch-draft-folder "drafts")
'(notmuch-fcc-dirs
(quote
(("jens.*@neuhalfen.name" . "jens@neuhalfen.name/Sent +sent -unread")
("JensNeuhalfen@gmx.de" . "jensneuhalfen@gmx.de/Sent +sent -unread")
(".*" . "EDIT-ME"))))
'(notmuch-maildir-use-notmuch-insert t)
'(sendmail-program "/usr/bin/msmtp")
'(session-use-package t nil (session))
'(user-mail-address "jens@neuhalfen.name"))
(custom-set-faces)

;;;; org-mode locations
(defconst org-directory "~/Documents/work/brabbler-ag/org-mode")

(defun my/append-path-part
(folder postfix)
"Concatenate the folder FOLDER with a POSTFIX (e.g. a filename)."
(concat (file-name-as-directory folder) postfix)
)

;; setup my agenda files
(defconst my-gtd-agenda-files (my/append-path-part org-directory "agenda"))
(defconst my-gtd-template-dir (my/append-path-part org-directory "_templates"))

;; define my file names relative to org-directory
(defconst my-gtd-inbox  (my/append-path-part my-gtd-agenda-files "inbox.org"))
(defconst my-gtd-reviews (my/append-path-part org-directory "reviews.org"))
(defconst my-gtd-tickler (my/append-path-part my-gtd-agenda-files "tickler.org"))
(defconst my-gtd-gtd (my/append-path-part my-gtd-agenda-files "gtd.org"))
(defconst my-gtd-someday (my/append-path-part org-directory "someday.org"))
(defconst my-gtd-journal (my/append-path-part org-directory "journal.org"))
(defconst my-gtd-meeting_notes (my/append-path-part org-directory "meeting_notes.org"))
(defconst my-browser-bookmarks (my/append-path-part org-directory "bookmarking.org"))


(defconst org-agenda-files (list my-gtd-gtd
my-gtd-inbox
my-gtd-tickler
my-gtd-meeting_notes))

;;; custom.el ends here
