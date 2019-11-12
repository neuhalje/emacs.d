;; init-unicode.el --- make emacs understand unicode

;;; Commentary:
;;; See https://www.emacswiki.org/emacs/UnicodeFonts

;;; Code:

(require-package 'unicode-fonts)
(unicode-fonts-setup)

(when *is-a-win*
  ;; scrolling with unicode is very slow else
  (setq inhibit-compacting-font-caches t)
  )

(provide 'init-unicode)

;;; init-unicode.el ends here
