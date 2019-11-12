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
 '(column-number-mode t)
 '(cua-mode t nil (cua-base))
 '(custom-enabled-themes (quote (sanityinc-tomorrow-bright)))
 '(menu-bar-mode nil)
 '(bookmark-automatically-show-annotations nil)
 '(session-use-package t nil (session))
 '(show-paren-mode t)
 '(tool-bar-mode nil))


(defun my/append-path-part
    (folder postfix)
  "Concatenate the folder FOLDER with a POSTFIX (e.g. a filename)."
  (concat (file-name-as-directory folder) postfix)
  )



(defconst my-org-context-private
  '(
    ;;;; red tape
    (my-org-context-name . "Privat")


;;;; org-mode locations
    (org-directory . (substitute-in-file-name "$USERPROFILE/Documents/work/private/org-mode"))

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


(defconst my-org-context-MY-PROJECT
  '(
;;;; red tape
    (my-org-context-name . "MY-PROJECT")

;;;; org-mode locations
    (org-directory . (substitute-in-file-name "$USERPROFILE/Documents/work/MY-PROJECT/org-mode"))

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

(defconst my-default-org-context my-org-context-private)

(defvar my-org-context-name)
;;;; org-mode locations
(defvar org-directory)

;; setup my agenda files
(defvar my-gtd-agenda-files)
(defvar my-gtd-template-dir)

;; define my file names relative to org-directory
(defvar my-gtd-inbox)
(defvar my-gtd-reviews)
(defvar my-gtd-tickler)
(defvar my-gtd-gtd)
(defvar my-gtd-someday)
(defvar my-gtd-journal)
(defvar my-gtd-meeting_notes)
(defvar my-browser-bookmarks)
(defvar org-agenda-file)

(require 'ido)
(defconst my-org-contexts
  '(("Privat" . my-org-context-private)
    ("my project" . my-org-context-MY-PROJECT)
    ))

;;;;; Bookmarking
(defun my-bookmark-file (bookmark-name bookmarked-file annotation)
  "Bookmark the file BOOKMARKED-FILE under the alias BOOKMARK-NAME.
The bookmark is annotated by ANNOTATION."
  ;; see defvar bookmark-alist () for documentation
  (let ((bookmark-v3-record `( ,bookmark-name . (
                                                 ( filename . ,bookmarked-file)
                                                 ( annotation . ,annotation)
                                                 ))))
    (bookmark-store bookmark-name bookmark-v3-record nil)
    )
  )

;;;;;; org-context logic

(defun my-set-org-context-by-name (context-name)
  "Set up the org-mode context."
  ;; take the context and set all variables defined in there
  (let ((context (eval context-name)))
    (loop for (key . value) in context
          do
          (set key (eval value))
          )
    (setq org-agenda-files (list my-gtd-gtd
                                 my-gtd-inbox
                                 my-gtd-tickler
                                 my-gtd-meeting_notes))

    (my-bookmark-file "gtd inbox" my-gtd-inbox my-org-context-name )
    (my-bookmark-file "gtd main" my-gtd-gtd my-org-context-name )
    (my-bookmark-file "gtd reviews" my-gtd-reviews my-org-context-name )
    (my-bookmark-file "gtd tickler" my-gtd-tickler my-org-context-name )
    (my-bookmark-file "gtd journal" my-gtd-journal my-org-context-name )

    (my-bookmark-file "browser bookmarks" my-browser-bookmarks my-org-context-name )
    )
  )

(defun my-pick-org-context ()
  "Prompt user to pick a org context from a list and return the name of the context variable."
  (interactive)
  (my-set-org-context-by-name (let (
                                    (choices (mapcar 'car my-org-contexts))
                                    )
                                (cdr (assoc (ido-completing-read  "Choose org context"  choices )  my-org-contexts))
                                )))


(my-set-org-context-by-name 'my-default-org-context)
(global-set-key (kbd "C-M-<f1>") 'my-pick-org-context)

;;;; Notmuch

(when *is-a-mac*
  (add-to-list 'load-path "/usr/local/share/emacs/site-lisp/notmuch")
  )
(when *is-a-win*
  ;;  (add-to-list 'load-path "//wsl$/Ubuntu/usr/share/emacs/site-lisp/elpa-src/notmuch-0.29.1")

  ;; On windows start emacs in daemon mode
  (require 'server)
  (unless (server-running-p)
    (server-start))
  )

(provide 'custom)
;;; custom.el ends here
