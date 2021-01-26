;;; org-header-heights.el -*- lexical-binding: t; -*-

;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Search_002dbased-Fontification.html
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Rx-Notation.html#Rx-Notation
;; https://github.com/alphapapa/org-sticky-header/issues/10#issuecomment-352464944
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Defining-Faces.html

(defface org-level-1-text '((t :height 2.4 :inherit org-level-1))
  "")

(defface org-level-2-text '((t :height 2.2 :inherit org-level-2))
  "")

(defface org-level-3-text '((t :height 2.0 :inherit org-level-3))
  "")

(defface org-level-4-text '((t :height 1.8 :inherit org-level-4))
  "")

(defface org-level-5-text '((t :height 1.4 :inherit org-level-5))
  "")

(defface org-level-6-text '((t :height 1.2 :inherit org-level-6))
  "")

(defface org-level-7-text '((t :height 1.0 :inherit org-level-7))
  "")

(defface org-level-8-text '((t :height 1.0 :inherit org-level-8))
  "")

(defvar org-level-text-faces
  '(org-level-1-text
    org-level-2-text
    org-level-3-text
    org-level-4-text
    org-level-5-text
    org-level-6-text
    org-level-7-text
    org-level-8-text))

(defun org-header-heights--get-level-text-face ()
  (let ((level (- (match-end 1)
                  (match-beginning 1)
                  1)))
    (nth level org-level-text-faces)))

(defvar org-header-heights--font-lock-keywords
  `((,org-heading-regexp (2 (org-header-heights--get-level-text-face)))))

(define-minor-mode +org-header-heights-mode
  ""
  :init-value nil

  (when (not (derived-mode-p 'org-mode))
    (+org-header-heights-mode -1)
    (error "org-mode is not enabled in this buffer."))

  (when +org-header-heights-mode
    (font-lock-add-keywords nil org-header-heights--font-lock-keywords))

  (unless +org-header-heights-mode
    (font-lock-remove-keywords nil org-header-heights--font-lock-keywords))

  (if (fboundp 'font-lock-flush)
      (font-lock-flush)
    (when font-lock-mode
      (with-no-warnings
        (font-lock-fontify-buffer)))))

(provide 'org-header-heights)

;;; org-header-heights.el ends here
