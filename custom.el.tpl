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
 '(bookmark-automatically-show-annotations nil)
 '(column-number-mode t)
 '(cua-mode t nil (cua-base))
 '(custom-enabled-themes (quote (sanityinc-tomorrow-bright)))
 '(message-sendmail-envelope-from nil)
 '(notmuch-draft-folder "drafts")
 '(notmuch-fcc-dirs
 (quote
 (("jens.*@example.com" . "jens@example.com/Sent +sent -unread")
 ("JensNeuhalfen@example.org" . "jensneuhalfen@example.org/Sent +sent -unread")
 (".*" . "EDIT-ME"))))
 '(notmuch-maildir-use-notmuch-insert t)
 '(sendmail-program "/usr/bin/msmtp")
 '(session-use-package t nil (session))
 '(user-mail-address "jens@example.com")

'(menu-bar-mode nil)
'(session-use-package t nil (session))
'(show-paren-mode t)
'(tool-bar-mode nil))

(defconst my-org-context-project-foo
  '(
    ;; org-mode locations
    (org-directory . (my-find-document-root-with-path "Documents/project-foo/org-mode") )

    ;;;;; setup my agenda files
    (my-gtd-agenda-files . org-directory)
    (my-gtd-template-dir . (my/append-path-part org-directory "_templates"))

    ;; define my file names relative to org-directory
    (my-gtd-inbox . (my/append-path-part my-gtd-agenda-files "inbox.org"))
    (my-gtd-reviews . (my/append-path-part org-directory "reviews.org"))
    (my-gtd-tickler . (my/append-path-part my-gtd-agenda-files "tickler.org"))
    (my-gtd-gtd . (my/append-path-part my-gtd-agenda-files "gtd.org"))
    (my-gtd-someday . (my/append-path-part org-directory "someday.org"))
    (my-gtd-journal . (my/append-path-part org-directory "journal.org"))
    (my-gtd-meeting_notes . (my/append-path-part org-directory "meeting_notes.org"))
    (my-browser-bookmarks . (my/append-path-part org-directory "browser_bookmarks.org"))
    )
  )

(defconst my-org-context-bar-product
  '(
    ;; org-mode locations
    (org-directory . (my-find-document-root-with-path "Documents/work/bar/org-mode") )

    ;; setup my agenda files
    (my-gtd-agenda-files . org-directory)
    (my-gtd-template-dir . (my/append-path-part org-directory "_templates"))

    ;; define my file names relative to org-directory
    (my-gtd-inbox . (my/append-path-part my-gtd-agenda-files "inbox.org"))
    (my-gtd-reviews . (my/append-path-part org-directory "reviews.org"))
    (my-gtd-tickler . (my/append-path-part my-gtd-agenda-files "tickler.org"))
    (my-gtd-gtd . (my/append-path-part my-gtd-agenda-files "gtd.org"))
    (my-gtd-someday . (my/append-path-part org-directory "someday.org"))
    (my-gtd-journal . (my/append-path-part org-directory "journal.org"))
    (my-gtd-meeting_notes . (my/append-path-part org-directory "meeting_notes.org"))
    (my-browser-bookmarks . (my/append-path-part org-directory "browser_bookmarks.org"))
    )
  )

(defconst my-org-context-private
  '(
    ;; org-mode locations
    (org-directory . (my-find-document-root-with-path "Documents/work/private/org-mode"))

    ;; setup my agenda files
    (my-gtd-agenda-files . org-directory)
    (my-gtd-template-dir . (my/append-path-part org-directory "_templates"))

    ;; define my file names relative to org-directory
    (my-gtd-inbox . (my/append-path-part my-gtd-agenda-files "inbox.org"))
    (my-gtd-reviews . (my/append-path-part org-directory "reviews.org"))
    (my-gtd-tickler . (my/append-path-part my-gtd-agenda-files "tickler.org"))
    (my-gtd-gtd . (my/append-path-part my-gtd-agenda-files "gtd.org"))
    (my-gtd-someday . (my/append-path-part org-directory "someday.org"))
    (my-gtd-journal . (my/append-path-part org-directory "journal.org"))
    (my-gtd-meeting_notes . (my/append-path-part org-directory "meeting_notes.org"))
    (my-browser-bookmarks . (my/append-path-part org-directory "browser_bookmarks.org"))
    )
  )

(defconst my-org-contexts
  `(("Privat" . ,my-org-context-private)
    ("Project foo" . ,my-org-context-project-foo)
    ("Bar" . ,my-org-context-bar-product)
    ))

(defconst my-default-org-context my-org-context-private )

;;;; Notmuch

(when *is-a-mac*
  (add-to-list 'load-path "/usr/local/share/emacs/site-lisp/notmuch")
  )
(when *is-a-win*
  ;;  (add-to-list 'load-path "//wsl$/Ubuntu/usr/share/emacs/site-lisp/elpa-src/notmuch-0.29.1")
)

(provide 'custom)
;;; custom.el ends here
