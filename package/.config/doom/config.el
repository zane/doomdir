;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Zane Shelby")
(setq user-mail-address "zaneshelby@gmail.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))
(setq doom-font "Input Mono-12")

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

;;; Basic
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-to-list 'initial-frame-alist '(fullscreen . maximized))
(add-to-list 'initial-frame-alist '(undecorated . t))

(after! newcomment
  (setq comment-style 'plain))

(after! evil
  (setq evil-move-beyond-eol t))

(after! projectile
  (setq projectile-project-search-path '("~/projects"))
  (projectile-add-known-project "~/.dotfiles"))

;;; Appearance
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar +theme-light 'gruvbox-light-medium)
(defvar +theme-dark 'gruvbox-dark-medium)

(defvar +default-sunrise-hour 5)
(defvar +default-sunset-hour 17)

(defvar +latitude 40.7)
(defvar +longitude -73.9)

(use-package! gruvbox-theme
  :if window-system
  :config
  (let* ((hour (decoded-time-hour (decode-time (current-time))))
         (theme (if (< +default-sunrise-hour hour +default-sunset-hour)
                    +theme-light
                  +theme-dark)))
    (load-theme theme t)))

(use-package! circadian
  :demand t
  :config
  (after! gruvbox-theme
    (setq calendar-latitude +latitude)
    (setq calendar-longitude +longitude)
    (setq circadian-themes '((:sunset  . gruvbox-dark-medium)
                             (:sunrise . gruvbox-light-medium)))
    (circadian-setup)))

;;; Lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package! clojure
  :mode ("\\.bb\\'" . clojure-mode))

(use-package! evil-lisp-state
  :demand t
  :custom (evil-lisp-state-cursor 'hollow)
  :init
  (setq evil-lisp-state-global t)
  :config
  (evil-lisp-state-leader "SPC k"))

(use-package! lispy
  :hook ((lisp-mode . lispy-mode)
         (emacs-lisp-mode . lispy-mode)
         (scheme-mode . lispy-mode)
         (racket-mode . lispy-mode)
         (hy-mode . lispy-mode)
         (lfe-mode . lispy-mode)
         (dune-mode . lispy-mode)
         (clojure-mode . lispy-mode))
  :config
  (lispy-set-key-theme '(evilcp))
  (define-key lispy-mode-map (kbd "y") nil)
  (define-key lispy-mode-map (kbd "[") 'lispy-brackets)
  (define-key lispy-mode-map (kbd "]") 'lispy-right))

(use-package! lispyville
  :when (featurep! :editor evil)
  :hook (lispy-mode . lispyville-mode)
  :init
  (setq lispyville-key-theme
        '((atom-movement normal visual)
          c-u
          c-w
          commentary
          (operators normal)
          (prettify insert)
          text-objects))
  :config
  (lispyville-set-key-theme))

;;; Org
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; https://www.reddit.com/r/emacs/comments/iemo44/wysiwygified_org_mode/
;; https://zzamboni.org/post/beautifying-org-mode-in-emacs/

(after! org
  (setq-default org-adapt-indentation nil))

(use-package! org-padding
  :after org
  :custom
  (org-padding-block-begin-line-padding  '(2.0 . nil))
  (org-padding-block-end-line-padding  '(nil . 1.0))
  (org-padding-heading-padding-alist  '((4.0 . 1.5)
                                        (3.0 . 0.5)
                                        (3.0 . 0.5)
                                        (3.0 . 0.5)
                                        (2.5 . 0.5)
                                        (2.0 . 0.5)
                                        (1.5 . 0.5)
                                        (0.5 . 0.5))))

(use-package! mixed-pitch
  :config
  (pushnew! mixed-pitch-fixed-pitch-faces
            'org-date
            'org-special-keyword
            'org-property-value
            'org-ref-cite-face
            'org-tag
            'org-todo-keyword-todo
            'org-todo-keyword-habt
            'org-todo-keyword-done
            'org-todo-keyword-wait
            'org-todo-keyword-kill
            'org-todo-keyword-outd
            'org-todo
            'org-done
            'font-lock-comment-face
            'line-number
            'line-number-current-line))

;;;###autoload
(define-minor-mode +org-word-processor-mode
  "Make org look like a word processor."
  :init-value nil
  :group 'evil-org
  (require 'face-remap)
  (require 'org-indent)
  (cond (+org-word-processor-mode
         (setq-local +org-word-processor--old-hl-line-mode (bound-and-true-p hl-line-mode))
         (setq-local +org-word-processor--old-org-hide-emphasis-markers (bound-and-true-p org-hide-emphasis-markers))
         (setq +org-word-processor--org-superstar-remove-leading-stars (bound-and-true-p org-superstar-remove-leading-stars))
         (setq +org-word-processor--old-org-indent-mode (bound-and-true-p org-indent-mode)
               +org-word-processor--old-org-superstar-mode (bound-and-true-p org-superstar-mode)
               +org-word-processor--old-mixed-pitch-mode (bound-and-true-p mixed-pitch-mode))
         (hl-line-mode -1)
         (setq org-hide-emphasis-markers t)
         (setq org-superstar-remove-leading-stars t)
         (org-indent-mode -1)
         (org-superstar-mode +1)
         (mixed-pitch-mode +1))
        (t
         (hl-line-mode +org-word-processor--old-hl-line-mode)
         (setq org-hide-emphasis-markers +org-word-processor--old-org-hide-emphasis-markers)
         (setq org-superstar-remove-leading-stars +org-word-processor--org-superstar-remove-leading-stars)
         (org-indent-mode (if +org-word-processor--old-org-indent-mode +1 -1))
         (org-superstar-mode (if +org-word-processor--old-org-superstar-mode +1 -1))
         (mixed-pitch-mode (if +org-word-processor--old-mixed-pitch-mode +1 -1)))))

;; https://emacs.stackexchange.com/questions/54932/is-there-a-way-to-add-some-margin-padding-above-a-line
;; https://github.com/volrath/treepy.el#main-differences-with-clojure-libraries
;; http://www.dr-qubit.org/predictive/auto-overlay-manual/html/index.html#Top
;;
;; Maybe what we do here is walk `font-lock-keywords' looking for a font face and
