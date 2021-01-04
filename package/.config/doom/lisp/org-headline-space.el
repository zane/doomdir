;;; org-headline-space.el -*- lexical-binding: t; -*-

(require 'org-element)
(require 'ov)

(defvar +org-headline-space--min-indent 10)
(defvar-local +org-headline-space--overlays '())
(defvar-local +org-headline-space--prev-org-indent-mode nil)

(defun +org-headline-space--reset ()
  (ov-reset +org-headline-space--overlays))

(defun +org-headline-space--ov-element (element)
  (ov-make (org-element-property :begin element)
           (org-element-property :end element)))

(defun +org-headline-space--refresh (begin end length)
  (ignore begin)
  (ignore end)
  (ignore length)
  (when +org-headline-space-mode
    (+org-headline-space--reset)
    (seq-doseq (element (org-element-map (org-element-parse-buffer) 'headline #'identity))
      (let* ((overlay (ov-line (org-element-property :begin element))))
        ;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Other-Display-Specs.html#Other-Display-Specs
        (ov-set overlay
                'before-string (propertize " "
                                           'display '(space . (:width 0
                                                               :relative-height 4
                                                               :ascent 100))))
        (setq +org-headline-space--overlays (cons overlay +org-headline-space--overlays))))))

(define-minor-mode +org-headline-space-mode
  ""
  :init-value nil

  (when (not (derived-mode-p 'org-mode))
    (error "`+org-headline-space-mode' can only be enabled in org-mode buffers"))

  (when +org-headline-space-mode
    (+org-headline-space--refresh nil nil nil)
    (org-add-hook 'after-change-functions '+org-headline-space--refresh nil 'local))

  (unless +org-headline-space-mode
    (+org-headline-space--reset)
    (remove-hook 'after-change-functions '+org-headline-space--refresh)))

;; FIXME
(defun +test ()
  (interactive)

  (let ((element (org-element-at-point)))
    (print
     (let* ((raw-value (org-element-property :raw-value element))
            (end (save-excursion
                   (goto-char (org-element-property :begin element))
                   (search-forward raw-value)
                   (point)))
            (start (- end (length raw-value))))
       (ov start end
           'face '(:height 2.0))))))


(provide 'org-headline-space)

;; https://practicaltypography.com/space-above-and-below.html

;;; org-headline-space.el ends here
