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
(setq doom-font (font-spec :family "Input Mono" :size 12))
(setq doom-variable-pitch-font (font-spec :family "Charter" :size 16))

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

(after! newcomment
  (setq comment-style 'plain))

(after! evil
  (setq evil-move-beyond-eol t))

(after! projectile
  (setq projectile-project-search-path '("~/projects"))
  (projectile-add-known-project "~/.dotfiles"))

(use-package! aggressive-indent
  :config
  (aggressive-indent-mode))

(use-package! atomic-chrome
  :after-call handle-focus-out
  :config
  (map! :mode atomic-chrome-edit-mode :n "ZZ" 'atomic-chrome-close-current-buffer)
  (setq atomic-chrome-default-major-mode 'markdown-mode)
  (setq atomic-chrome-buffer-open-style 'frame)
  (atomic-chrome-start-server))

;;; Appearance
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar +theme-light 'doom-gruvbox-light)
(defvar +theme-dark 'doom-gruvbox)

(defun +soften-org-block-lines ()
  "Change org-block faces to be faint text against the default background."
  (interactive)
  (let ((color  (doom-blend (face-foreground 'default)
                            (face-background 'default)
                            0.2)))
    (custom-set-faces!
      `(org-block-begin-line :foreground ,color :background nil)
      `(org-block-end-line :foreground ,color :background nil))))

(defun +fix-cursor-color ()
  "Changes the cursor color to be the same color as the default foreground color."
  ;; Not every theme defines a cursor color. This can cause problems when using
  ;; a light theme because the default cursor color is `"#ffffff"'.
  (let ((cursor-color (face-foreground 'default)))
    (custom-set-faces! `(cursor :background ,cursor-color))
    (setq +evil--default-cursor-color cursor-color)))

(defun +apply-theme (appearance)
  "Load theme and enable our theme-agnostic tweaks, taking current system appearance into consideration."
  (mapc #'disable-theme custom-enabled-themes)
  (pcase appearance
    ('light (load-theme +theme-light t))
    ('dark (load-theme +theme-dark t)))
  (+soften-org-block-lines)
  (+fix-cursor-color))

(when IS-MAC
  ;; https://github.com/d12frosted/homebrew-emacs-plus#system-appearance-change
  (add-hook 'ns-system-appearance-change-functions #'+apply-theme)
  (+apply-theme ns-system-appearance))

(after! magit-todos
  (magit-todos-mode +1)) ; show todos and the like in magit by default

(after! simple
  ;; Navigate via visual line when `visual-line-mode' is enabled.
  (map! :map visual-line-mode-map
        :m "j" #'evil-next-visual-line
        :m "k" #'evil-previous-visual-line
        :m "<down>" #'evil-next-visual-line
        :m "<up>" #'evil-previous-visual-line))

;;; Lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package! clojure
  :mode ("\\.bb\\'" . clojure-mode)
  :config)

(after! cider
  (set-repl-handler! 'clojurec-mode #'+clojure/open-repl :persist t)
  (set-eval-handler! 'clojurec-mode #'cider-eval-region))

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
  (define-key lispy-mode-map (kbd ";") nil)
  (define-key lispy-mode-map (kbd "y") nil)
  (define-key lispy-mode-map (kbd "<") nil)
  (define-key lispy-mode-map (kbd ">") nil)
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
