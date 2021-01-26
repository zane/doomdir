;;; prettify-org.el -*- lexical-binding: t; -*-

;;; The overall approach here is to define a minor mode `prettify-org-mode'.
;;; When that mode is enabled we create font-lock keywords, store them in the
;;; variable `prettify-org--keywords', and then attach them to the text via
;;; font-locking. Those font-lock keywords attach text decorations that hide or
;;; otherwise rewrite text in the buffer.

;;; We then define a function and add it to `post-command-hook' that removes the
;;; text decorations if the point is near (Or inside? This part needs to be
;;; fleshed out a bit) the affected region.

;;; FIXME If memory serves, the above approach isn't going to work. I'm better
;;; off following the approach used by `pretty-mode', or by configuring
;;; `pretty-mode' (or the built-in `prettify-symbols-mode') itself.

;;; Update: Deeeefinitely don't want to use the above. Probably want to use
;;; the text decoration / overlay hooks.

(defun prettify-org--make-keywords ()
  nil)

(defvar-local prettify-org--keywords nil)

(define-minor-mode prettify-org-mode
  ""
  :init-value nil

  (when prettify-org--keywords
    (font-lock-remove-keywords nil prettify-org--keywords)
    (setq prettify-org--keywords nil))

  (when prettify-org-mode
    (when (setq prettify-org--keywords (prettify-org--make-keywords)))
    (message "on"))

  (unless prettify-org-mode
    (message "off")))

;; TODO https://www.reddit.com/r/emacs/comments/kmjkoi/n%CE%BBno_writer_mode/
;; TODO https://www.reddit.com/r/orgmode/comments/j6hlo3/how_to_get_better_org_indentation/
;; TODO https://github.com/emacsorphanage/ov

(provide 'prettify-org)

;;; prettify-org.el ends here
