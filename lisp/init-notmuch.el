;;; init-notmuch.el --- configure notmuch mail

;;; Commentary:
;;; Use the notmuch version your OS provides - the (M)ELPA version very likely
;;; does not match the binary.


;;; Code:



(when (not *is-a-win*)

;; Install notmuch via the operating system to keep it synchronized with the
;; lib!
(when *is-a-mac*
  (add-to-list 'load-path "/usr/local/share/emacs/site-lisp/notmuch")
  )
;;
;; (when *is-a-win* (add-to-list 'load-path "//wsl$/Ubuntu/usr/share/emacs/site-lisp/elpa-src/notmuch-0.26"))
;;

;; Mail
(require 'notmuch)

;; Sending e-mail
(setq mail-specify-envelope-from t)
(setq send-mail-function (quote sendmail-send-it))

;; Notmuch
(setq mm-inline-large-images (quote resize))
(setq notmuch-archive-tags (quote ("-inbox" "+archive" "-fyi" "-todo" "-unread")))
(setq notmuch-hello-sections
      (quote
       (notmuch-hello-insert-saved-searches notmuch-hello-insert-search notmuch-hello-insert-recent-searches notmuch-hello-insert-alltags notmuch-hello-insert-footer)))
(setq notmuch-saved-searches
      (quote
       ((:name "inbox" :query "tag:inbox" :key "i")
        (:name "fyi" :query "is:fyi and not is:todo" :key "y")
        (:name "unread" :query "tag:unread" :key "u")
        (:name "flagged" :query "tag:flagged" :key "f")
        (:name "Waiting for others" :query "is:waiting_for_others" :key "w")
        (:name "sent - to be archived" :query "is:sent and (is:inbox or not is:archive)" :key "S")
        (:name "todo" :query "is:todo" :key "t")
        (:name "sent" :query "tag:sent" :key "s")
        (:name "drafts" :query "tag:draft" :key "d")
        (:name "all mail" :query "*" :key "a"))))
(setq notmuch-tag-formats
      (quote
       (("archive"
         #("A" 0 1
           (face
            ((:weight ultra-bold :foreground "dim gray"))))
         (notmuch-apply-face tag
                             (quote
                              (:weight ultra-bold :foreground "dim gray"))))
        ("important"
         #("!" 0 1
           (face
            ((:weight ultra-bold :foreground "red"))))
         (notmuch-apply-face tag
                             (quote
                              (:weight ultra-bold :foreground "red"))))
        ("signed")
        ("work")
        ("to-me")
        ("me")
        ("replied"
         #("↻" 0 1
           (face
            ((:weight ultra-bold :foreground "light gray"))))
         (notmuch-apply-face tag
                             (quote
                              (:weight ultra-bold :foreground "light gray"))))
        ("unread" "✉️")
        ("attachment" "📎")
        ("inbox" "📥")
        ("sent"
         #("➥" 0 1
           (face
            ((:weight bold :foreground "light gray"))))
         (notmuch-apply-face tag
                             (quote
                              (:weight bold :foreground "light gray"))))
        ("folder/.*")
        ("vorstand")
        ("DPAG.*"
         (notmuch-apply-face tag
                             (quote
                              (:foreground "gold"))))
        ("system/.*"
         (notmuch-apply-face tag
                             (quote
                              (:foreground "dim gray"))))
        ("personnel.*"
         (notmuch-apply-face tag
                             (quote
                              (:foreground "medium spring green"))))
        ("fyi"
         (notmuch-apply-face tag
                             (quote
                              (:foreground "dim gray"))))
        ("account/brabbler")
        ("unread"
         (propertize tag
                     (quote face)
                     (quote notmuch-tag-unread)))
        ("waiting_for_others"
         (notmuch-apply-face tag
                             (quote
                              (:foreground "deep sky blue"))))
        ("todo"
         (notmuch-apply-face tag
                             (quote
                              (:foreground "dark orange"))))
        ("flagged"
         (notmuch-tag-format-image-data tag
                                        (notmuch-tag-star-icon))
         (propertize tag
                     (quote face)
                     (quote notmuch-tag-flagged))))))
(setq notmuch-tagging-keys
      (quote
       (("a" notmuch-archive-tags "Archive")
        ("u" notmuch-show-mark-read-tags "Mark read")
        ("f"
         ("+flagged")
         "Flag")
        ("s"
         ("+spam" "-inbox")
         "Mark as spam")
        ("d"
         ("+deleted")
         "Delete")
        ("w"
         ("+waiting_for_others")
         "Waiting for others"))))


;; Start notmuch via shortcut
(global-set-key (kbd "C-c m") `notmuch)

;; do not kill buffer after sending mail to e.g. get mail-links
(setq message-kill-buffer-on-exit nil)



(define-key notmuch-search-mode-map "d"
  (lambda ()
    "toggle deleted tag for thread"
    (interactive)
    (if (member "deleted" (notmuch-search-get-tags))
        (notmuch-search-tag "-deleted")
      (notmuch-search-tag '("+deleted" "-inbox" "-unread")))
    (next-line)))
(define-key notmuch-show-mode-map "d"
  (lambda ()
    "toggle deleted tag for message"
    (interactive)
    (if (member "deleted" (notmuch-show-get-tags))
        (notmuch-show-tag "-deleted")
      (notmuch-show-tag '("+deleted" "-inbox" "-unread")))))

)

(provide 'init-notmuch)
;;; init-notmuch.el ends here
