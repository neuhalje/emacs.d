;;; init-local --- Local initialisation files

;;; Commentary:
;;; My custom configuration
;;;
;;; custom.el should contain the following:
;;;
;;;; (defconst my-org-context-private
;;;;   '(
;;;;     ;; org-mode locations
;;;;     (org-directory . (my-find-document-root-with-path "Documents/work/private/org-mode"))
;;;;
;;;;     ;; setup my agenda files
;;;;     (my-gtd-agenda-files . org-directory)
;;;;     (my-gtd-template-dir . (my/append-path-part org-directory "_templates"))
;;;;
;;;;     ;; define my file names relative to org-directory
;;;;     (my-gtd-inbox . (my/append-path-part my-gtd-agenda-files "inbox.org"))
;;;;     (my-gtd-reviews . (my/append-path-part org-directory "reviews.org"))
;;;;     (my-gtd-tickler . (my/append-path-part my-gtd-agenda-files "tickler.org"))
;;;;     (my-gtd-gtd . (my/append-path-part my-gtd-agenda-files "gtd.org"))
;;;;     (my-gtd-someday . (my/append-path-part org-directory "someday.org"))
;;;;     (my-gtd-journal . (my/append-path-part org-directory "journal.org"))
;;;;     (my-gtd-meeting_notes . (my/append-path-part org-directory "meeting_notes.org"))
;;;;     (my-browser-bookmarks . (my/append-path-part org-directory "browser_bookmarks.org"))
;;;;     )
;;;;   )
;;;; (defconst my-org-context-project-foo ...)
;;;; (defconst my-org-context-project-bar ...)
;;;;
;;;; (defconst my-org-contexts
;;;;   `(("Privat" . ,my-org-context-private)
;;;;     ("Project FOO" . ,my-org-context-project-foo)
;;;;     ("Project bar" . ,my-org-context-project-bar)
;;;;     ))
;;;;
;;;; (defconst my-default-org-context my-org-context-private )
;;;;

;;; Code:

(defun my/append-path-part
    (folder postfix)
  "Concatenate the folder FOLDER with a POSTFIX (e.g. a filename)."
  (concat (file-name-as-directory folder) postfix)
  )

(defun my-document-root-expand (path contains-path)
  (let* (
         (joined-path (concat (file-name-as-directory path) contains-path))
         (expanded-path (expand-file-name (substitute-env-in-file-name joined-path))))
    expanded-path
    )
  )

(defconst my-possible-document-roots (list "~" "%HOME" "$HOME" "$USERPROFILE" "$APPDATA" "/mnt/c/users/$USER"))

(defun my-find-document-root-with-path (with-postfix-path)
  "Return a possible document root, e.g. ~/Documents that contains inside it a sub-path WITH-POSTFIX-PATH."
  (loop for candidate in my-possible-document-roots
        for expanded-path =   (my-document-root-expand candidate with-postfix-path)
        when ( file-directory-p expanded-path)
        do (return expanded-path) )
  )

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
(require 'bookmark)

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

(defun my-set-org-context-by-alist (context)
  "Set up the org-mode CONTEXT."
  ;; take the context and set all variables defined in there
  (loop for (key . value) in context
        do
        (print (format "Setting %s => %s" key value))
        (set key (eval value))
        )
  (setq org-agenda-files (list my-gtd-gtd
                               my-gtd-inbox
                               my-gtd-tickler
                               my-gtd-meeting_notes))

  (my-bookmark-file "gtd inbox" my-gtd-inbox "Auto generated" )
  (my-bookmark-file "gtd main" my-gtd-gtd "Auto generated")
  (my-bookmark-file "gtd reviews" my-gtd-reviews "Auto genetrated")
  (my-bookmark-file "gtd tickler" my-gtd-tickler "Auto generated")
  (my-bookmark-file "gtd journal" my-gtd-journal "Auto generated")

  (my-bookmark-file "browser bookmarks" my-browser-bookmarks "Auto generated")
  )

(defun my-pick-org-context ()
  "Prompt user to pick a org context from a list and return the name of the context variable."
  (interactive)
  (my-set-org-context-by-alist (let* (
                                      (choices (mapcar 'car my-org-contexts))
                                      (context (cdr (assoc (ido-completing-read  "Choose org context:"  choices )  my-org-contexts)))
                                      )
                                 context
                                 )))


(my-set-org-context-by-alist my-default-org-context)
(global-set-key (kbd "C-M-<f1>") 'my-pick-org-context)

(provide 'init-local)
;;; init-local.el ends here
