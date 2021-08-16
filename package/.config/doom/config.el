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
(setq org-directory "~/Dropbox/org/")

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

(defun doom/ediff-init-and-example ()
  "ediff the current `init.el' with the example in doom-emacs-dir"
  (interactive)
  (ediff-files (concat doom-private-dir "init.el")
               (concat doom-emacs-dir "init.example.el")))

(add-to-list 'initial-frame-alist '(fullscreen . maximized))

(after! core-ui
  (setq global-hl-line-modes (delete 'text-mode global-hl-line-modes)))

(after! files
  (setq confirm-kill-processes nil)) ; don't ask

(after! newcomment
  (setq comment-auto-fill-only-comments t)
  (add-hook! prog-mode #'auto-fill-mode))

(after! evil
  (setq evil-move-beyond-eol nil)
  (setq evil-respect-visual-line-mode t)
  ;; This is to work around a bug. Without the following line when changing from
  ;; normal mode into insert mode the cursor is changed from bar to line, but
  ;; the bar is still visible. This gives the appearance that Emacs is still in
  ;; normal mode.
  (add-hook! 'evil-insert-state-entry-hook #'redisplay))

(after! projectile
  (setq projectile-indexing-method 'alien)
  (let ((projects-dir "~/projects"))
    (when (file-directory-p projects-dir)
      (setq projectile-project-search-path (list projects-dir)))))

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

(after! flycheck
  (map! :n "[e" 'flycheck-previous-error)
  (map! :n "]e" 'flycheck-next-error))

(after! ranger
  (setq ranger-show-hidden t))

;;; Appearance
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar +theme-light 'doom-gruvbox-light)
(defvar +theme-dark 'doom-gruvbox)

(defun z/soften-org-block-lines ()
  "Change org-block faces to be faint text against the default background."
  (interactive)
  (let ((color  (doom-blend (face-foreground 'default)
                            (face-background 'default)
                            0.2)))
    (custom-set-faces!
      `(org-block-begin-line :foreground ,color :background nil)
      `(org-block-end-line :foreground ,color :background nil))))

(after! evil
  (defun z/default-cursor-fn ()
    (evil-set-cursor-color (face-foreground 'font-lock-keyword-face)))
  (setq evil-default-cursor 'z/default-cursor-fn))

(defun z/apply-theme (appearance)
  "Load theme and enable our theme-agnostic tweaks, taking current system
appearance into consideration."
  (mapc #'disable-theme custom-enabled-themes)
  (pcase appearance
    ('light (load-theme +theme-light t))
    ('dark (load-theme +theme-dark t)))
  (z/soften-org-block-lines))

(defun z/sync-theme ()
  (interactive)
  (when IS-MAC
    (cond
     ;; emacs-plus
     ((boundp 'ns-system-appearance)
      (progn
        ;; https://github.com/d12frosted/homebrew-emacs-plus#system-appearance-change
        (add-hook! 'ns-system-appearance-change-functions #'z/apply-theme)))

     ;; emacs-mac
     ((fboundp 'mac-application-state)
      (let ((appearance (pcase (plist-get (mac-application-state) :appearance)
                          ("NSAppearanceNameAqua" 'light)
                          ("NSAppearanceNameDarkAqua" 'dark))))
        (z/apply-theme appearance)))
     (z/apply-theme 'dark))))

(after! writeroom-mode
  (setq writeroom-global-effects nil)
  (setq writeroom-maximize-window nil))

;;; JavaScript

(defun z/set-js-mode-indent-width ()
  (setq js-expr-indent-offset -2)
  (doom/set-indent-width 2))

(after! js
  (setq js-expr-indent-offset -2)
  (setq js-chain-indent nil))

(add-hook! js-mode #'z/set-js-mode-indent-width)

(z/sync-theme)

(after! magit
  (setq magit-branch-prefer-remote-upstream t))

(after! magit-todos
  (magit-todos-mode +1)) ; show todos and the like in magit by default

(after! simple
  ;; Navigate via visual line when `visual-line-mode' is enabled.
  (map! :map visual-line-mode-map
        :m "$" #'evil-end-of-visual-line
        :m "j" #'evil-next-visual-line
        :m "k" #'evil-previous-visual-line
        :m "<down>" #'evil-next-visual-line
        :m "<up>" #'evil-previous-visual-line))

;;; Calendar
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun z/open-calendar ()
  (interactive)
  (cfw:open-calendar-buffer
   :contents-sources
   (list
    (cfw:ical-create-source "zaneshelby@gmail.com" "https://calendar.google.com/calendar/ical/zaneshelby%40gmail.com/private-55dd7bd22292ba60b84f40f441900bdb/basic.ics" ;; (face-foreground 'default)
                            ))))

;;; Editor
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(after! format-all
  (add-hook 'python-mode-hook #'format-all-mode))

;;; LSP
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(after! lsp-ui
  ;; https://emacs-lsp.github.io/lsp-mode/tutorials/how-to-turn-off/
  (setq lsp-enable-symbol-highlighting nil)
  (setq lsp-log-io t)
  (setq lsp-ui-sideline-enable nil))

;;; Python
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun +fill-column-to-79 ()
  (set-fill-column 79))

(after! python
  (add-hook! python-mode #'+fill-column-to-79))

;;; HTML
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package web-mode
  :custom
  (web-mode-attr-indent-offset nil)
  (web-mode-attr-value-indent-offset nil)
  (web-mode-block-padding 2)
  (web-mode-code-indent-offset 2)
  (web-mode-css-indent-offset 2)
  (web-mode-markup-indent-offset 2)
  (web-mode-script-padding 2)
  (web-mode-style-padding 2))

;;; Lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package! clojure
  :mode ("\\.bb\\'" . clojure-mode))

(after! files
  (put 'cider-clojure-cli-global-options 'safe-local-variable #'stringp))

(after! clojure-mode
  (put-clojure-indent 'prop/for-all 1)
  (put-clojure-indent 'thrown? 1)
  (put-clojure-indent 'comment -3))

(defun string-nil-or-empty-p (s)
  (or (not s)
      (string= "" s)))

(defun +zane/disable-dynapath (params)
  (setq cider-clojure-cli-global-options
        (concat cider-clojure-cli-global-options
                (unless (string-nil-or-empty-p cider-clojure-cli-global-options)
                  " ")
                "-J-Dorchard.use-dynapath=false")))

(after! cider
  (set-popup-rule! "^\\*cider-repl" :quit nil :ttl nil)
  (set-repl-handler! 'clojurec-mode #'+clojure/open-repl :persist t)
  (set-eval-handler! 'clojurec-mode #'cider-eval-region)
  ;; https://github.com/clojure-emacs/orchard/pull/112
  (advice-add 'cider-jack-in-clj :before #'+zane/disable-dynapath))

(add-hook! clojurec-mode #'cider-mode)

(after! (evil smartparens)
  (evil-define-text-object +zane:inner-sp-sexp-texobj (count &optional beg end type)
    (evil-range
     (save-excursion
       (sp-forward-sexp)
       (point))
     (save-excursion
       (sp-forward-sexp)
       (sp-backward-sexp)
       (point))
     type))
  (map! :textobj "x" #'+zane:inner-sp-sexp-texobj #'+zane:inner-sp-sexp-texobj))

(use-package! evil-lisp-state
  :demand t
  :init
  (setq evil-lisp-state-cursor 'hollow)
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
  (define-key lispy-mode-map (kbd "C-a") nil)
  (define-key lispy-mode-map (kbd "[") 'lispy-brackets)
  (define-key lispy-mode-map (kbd "]") 'lispy-right)
  (define-key lispy-mode-map (kbd "}") 'lispy-right))

(use-package! lispyville
  :when (featurep! :editor evil)
  :hook (lispy-mode . lispyville-mode)
  :init
  (setq lispyville-key-theme
        '(c-u
          c-w
          commentary
          operators))
  :config
  (lispyville-set-key-theme))

;;; Eval
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Override `+eval/region' so that it doesn't insert evaluated text into the
;; REPL buffer even if the REPL buffer is open.

(defun +simple-eval/region (beg end)
  "Evaluate a region between BEG and END and display the output. Do not insert
into the REPL buffer, even if it is open."
  (interactive "r")
  (let ((load-file-name buffer-file-name))
    (if-let (runner (alist-get major-mode +eval-runners))
        (funcall runner beg end)
      (quickrun-region beg end))))

(advice-add '+eval/region :override #'+simple-eval/region)

;;; Org
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(after! mixed-pitch
  (setq mixed-pitch-set-height nil))

(defun +hide-line-numbers ()
  "Hide line numbers."
  (setq display-line-numbers nil))

(after! org
  (setq org-adapt-indentation nil)
  (setq org-hide-leading-stars nil)
  (add-hook! org-mode #'+hide-line-numbers))

(after! (:and org org-src)
  (map! :mode org-src-mode :n "ZZ" 'org-edit-src-exit)) ; Have mixed-pitch use the font from `doom-variable-pitch-font'.

(after! writeroom-mode
  (setq +zen-text-scale 0))

(use-package! prettify-org
  :after org
  :load-path "lisp")

(use-package! org-roam
  :after org
  :config
  (setq org-roam-directory (file-truename "~/Dropbox/org/roam/")))

(use-package! org-margin-stars
  :after org
  :load-path "lisp")

(use-package! org-header-heights
  :after org
  :load-path "lisp")

(use-package! org-headline-space
  :after org
  :load-path "lisp")

(after! org-tree-slide
  (setq org-tree-slide-fold-subtrees-skipped nil)
  (setq org-tree-slide-skip-outline-level 0))
