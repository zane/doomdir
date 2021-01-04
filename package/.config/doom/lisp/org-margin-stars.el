;;; org-margin-stars.el -*- lexical-binding: t; -*-

(require 'org-element)
(require 'ov)

(defun +org-margin-stars--leading-chars (element)
  "Returns the number of leading characters in ELEMENT that should be in the
margin."
  (pcase (org-element-type element)
    ('headline (+ 1 ; for the space between the headline and the stars
                  (org-element-property :level element)))
    ('keyword (+ 2 ; #+
                 2 ; :\space
                 (length (org-element-property :key element))))
    (_ 0)))

(defun +org-margin-stars--longest ()
  "Returns the longest headline element in the current buffer."
  (interactive)
  (let ((leading-chars (org-element-map (org-element-parse-buffer) '(headline keyword)
                         #'+org-margin-stars--leading-chars)))
    (if (seq-empty-p leading-chars)
        0
      (seq-max leading-chars))))

(defvar +org-margin-stars--min-indent 10)
(defvar-local +org-margin-stars--overlays '())

(defun +org-margin-stars--reset ()
  (ov-reset +org-margin-stars--overlays))

(defun +org-margin-stars--refresh (begin end length)
  (ignore begin)
  (ignore end)
  (ignore length)
  (when +org-margin-stars-mode
    (+org-margin-stars--reset)
    (let ((indent-level (max +org-margin-stars--min-indent
                             (+org-margin-stars--longest))))
      (save-excursion
        (goto-char (point-min))
        (while (not (eobp))
          (let* ((additional-space (- indent-level
                                      (+org-margin-stars--leading-chars (org-element-at-point))))
                 (prefix `(space . (:width ,additional-space)))
                 (overlay (ov-line)))
            (ov-set overlay
                    'line-prefix prefix
                    'wrap-prefix prefix)
            (setq +org-margin-stars--overlays (cons overlay +org-margin-stars--overlays)))
          (forward-line))))))

(define-minor-mode +org-margin-stars-mode
  "Minor mode that moves leading meadata, like headline stars and export
settings into the left margin."
  :init-value nil

  (when (not (derived-mode-p 'org-mode))
    (error "`+org-margin-stars-mode' can only be enabled in org-mode buffers"))

  (when (derived-mode-p 'org-indent-mode)
    (error "`+org-margin-stars-mode' cannot be used with `org-indent-mode'"))

  (when +org-margin-stars-mode
    (+org-margin-stars--refresh nil nil nil)
    (org-add-hook 'after-change-functions '+org-margin-stars--refresh nil 'local))

  (unless +org-margin-stars-mode
    (+org-margin-stars--reset)
    (remove-hook 'after-change-functions '+org-margin-stars--refresh)))

;; FIXME
(defun +org-element-at-point ()
  (interactive)
  (print (org-element-at-point)))

;; FIXME
(defun +test ()
  (interactive)
  (print (window-text-pixel-size (selected-window) (region-beginning) (region-end))))

;; TODO https://www.reddit.com/r/emacs/comments/kmjkoi/n%CE%BBno_writer_mode/

;; BUG: Overlays get blown away when yasnippets are expanded

;; TODO Support mixed-pitch via pixels?
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Pixel-Specification.html#Pixel-Specification

(provide 'org-margin-stars)

;;; org-margin-stars.el ends here
