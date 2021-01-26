;;; org-margin-stars.el -*- lexical-binding: t; -*-

(require 'org-element)

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
  (let ((leading-chars (org-element-map (org-element-parse-buffer 'greater-element)
                           '(headline keyword)
                         #'+org-margin-stars--leading-chars)))
    (if (seq-empty-p leading-chars)
        0
      (seq-max leading-chars))))

(defvar +org-margin-stars--min-margin 10)

(defun +org-margin-stars--margin-width ()
  (max +org-margin-stars--min-margin (+org-margin-stars--longest)))

(defun +org-margin-stars--line-prefix ()
  (let* ((margin-width (+org-margin-stars--margin-width))
         (prefix-width (- margin-width
                          (if (eq (line-beginning-position) (line-end-position))
                              0
                            (+org-margin-stars--leading-chars
                             (save-excursion
                               (beginning-of-line)
                               (org-element-at-point)))))))
    (make-string prefix-width ?\s)))

(defun +org-margin-stars--set-window-margins ()
  (set-window-margins left-margin-width (+org-margin-stars--margin-width)))

(defun +org-margin-stars--add-line-properties ()
  (interactive)
  (let ((prefix (+org-margin-stars--line-prefix)))
    (add-text-properties (line-beginning-position)
                         (line-beginning-position 2)
                         `(line-prefix ,prefix wrap-prefix ,prefix))))

(defun +org-margin-stars--add-properties (beg end)
  (org-with-wide-buffer
   (goto-char beg)
   (beginning-of-line)
   (with-silent-modifications
     (while (and (<= (point) end)
                 (not (eobp)))
       (+org-margin-stars--add-line-properties)
       (forward-line)))))

(defun +org-margin-stars--remove-properties (beg end)
  (interactive)
  (with-silent-modifications
    (remove-text-properties beg end '(line-prefix nil wrap-prefix nil))))

(defun +org-margin-stars--refresh-properties (beg end len)
  (ignore beg)
  (ignore end)
  (ignore len)
  ;; TODO: Actually use `beg' and `end'.
  (save-match-data
    (+org-margin-stars--remove-properties (point-min) (point-max))
    (+org-margin-stars--add-properties (point-min) (point-max))))

(defun +org-margin-stars--refresh-margins ()
  (set-window-margins nil
                      (car (window-margins))
                      (+org-margin-stars--margin-width)))

(defun +org-margin-stars--refresh-all (beg end len)
  (+org-margin-stars--refresh-margins)
  (+org-margin-stars--refresh-properties beg end len))

(defvar-local +org-margin-stars--orig-window-margins nil)

(define-minor-mode +org-margin-stars-mode
  "Minor mode that moves leading meadata, like headline stars and export
settings into the left margin."
  :init-value nil

  (when (not (derived-mode-p 'org-mode))
    (error "`+org-margin-stars-mode' can only be enabled in org-mode buffers"))

  (when (derived-mode-p 'org-indent-mode)
    (error "`+org-margin-stars-mode' cannot be used with `org-indent-mode'"))

  (when +org-margin-stars-mode
    (setq-local +org-margin-stars--orig-window-margins (window-margins))
    (+org-margin-stars--refresh-all (point-min) (point-max) nil)
    (org-add-hook 'after-change-functions '+org-margin-stars--refresh-all nil 'local))

  (unless +org-margin-stars-mode
    (set-window-margins (car +org-margin-stars--orig-window-margins)
                        (cdr +org-margin-stars--orig-window-margins))
    (org-with-wide-buffer
     (+org-margin-stars--remove-properties (point-min) (point-max)))
    (remove-hook 'after-change-functions '+org-margin-stars--refresh-all 'local)))

;; https://www.reddit.com/r/emacs/comments/kmjkoi/n%CE%BBno_writer_mode/
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Pixel-Specification.html#Pixel-Specification
;;
;; BUG: Overlays get blown away when yasnippets are expanded
;; TODO: Assign a face to the margin characters, have it inherit from fixed-pitch
;; TODO: Support mixed-pitch via pixels?

(provide 'org-margin-stars)

;;; org-margin-stars.el ends here
