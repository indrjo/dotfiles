
;;;
;;;
;;;


;;
;; Sources that made possible this configuration file:
;;
;; >>> https://github.com/daviwil/emacs-from-scratch
;; >>> https://www.youtube.com/watch?v=74zOY-vgkyw
;; >>> https://emacsrocks.com/
;;

(setq inhibit-startup-message t)
(tool-bar-mode -1)
(tooltip-mode -1)
(set-fringe-mode 0)

;; Set up the visible bell
;(setq visible-bell t)

;; Show line numbers...
(column-number-mode)
(global-display-line-numbers-mode t)
;; ... except for certain modes
(dolist
    (mode '(term-mode-hook
            shell-mode-hook
            treemacs-mode-hook
            eshell-mode-hook))
  (add-hook mode
            (lambda ()
              (display-line-numbers-mode 0))))

;; Show the current column number too.
(setq-default column-number-mode t)

;; Highlight current row.
(global-hl-line-mode t)

;; Insert spaces when you hit tab.
(setq-default tab-width 2)
(setq-default indent-tabs-mode nil)

;; The list of package archives.
(require 'package)
(setq package-archives
      '(("elpa"         . "https://elpa.gnu.org/packages/")
        ("non-gnu"      . "https://elpa.nongnu.org/nongnu/")
        ("melpa"        . "https://melpa.org/packages/")
        ("melpa-stable" . "https://stable.melpa.org/packages/")
        ("org"          . "https://orgmode.org/elpa/")))

(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

;; Make sure `use-package` is installed. It will be used to install missing
;; packages and configure them.
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;; A directory to be used as a sandbox and not to mess the GNU Emacs up.
(setq custom-dir "~/.emacs.d/custom/")
(unless (file-exists-p custom-dir)
  (make-directory custom-dir))
(add-to-list 'load-path custom-dir)

;; Help keeping ~/.config/emacs clean.
(setq user-emacs-directory "~/.cache/emacs")

(use-package no-littering)
(setq auto-save-file-name-transforms
      `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))

;; Show events or command history of certain buffers.
(use-package command-log-mode)

;; This package assists you in narrowing and picking the right string from
;; a vast number of choices in certain buffers. Stolen.
(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)
         ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
	 ("C-d" . ivy-reverse-i-search-kill))
  :config (ivy-mode 1))

;; The selected theme.
(use-package darcula-theme
  :ensure t)
(load-theme 'darcula t)

(use-package all-the-icons)
(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 15)))

;; A library to make sure the environment variables inside GNU Emacs are
;; the same of your shell. For reference see:
;; >>> https://github.com/purcell/exec-path-from-shell#usage
(use-package exec-path-from-shell)
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))


;;; LANGUAGES

;; Visit also: https://emacs-lsp.github.io/lsp-mode/page/installation/.
(use-package lsp-mode
  :init (setq lsp-keymap-prefix "C-c l")
  :hook ((python-mode  . lsp)
         (haskell-mode . lsp)
         (racket-mode  . lsp))
  :commands lsp)

(use-package lsp-ui
  :commands lsp-ui-mode)

(use-package lsp-treemacs
  :commands lsp-treemacs-errors-list)

(use-package lsp-ivy
  :commands lsp-ivy-workspace-symbol)

(use-package which-key
    :config (which-key-mode))

(use-package flycheck
  :ensure t
  :config (add-hook 'after-init-hook 'global-flycheck-mode))

;; Completions
(use-package company)
(add-hook 'after-init-hook 'global-company-mode)

;; Snippets
(use-package yasnippet)
;; add the path of some personal snippets
;; !!! my snippets will override the others.
(setq yas-snippet-dirs (cons "~/snippets/emacs" yas-snippet-dirs))
;; a good collection of snippets for many languages, good to have.
(use-package yasnippet-snippets)
(yas-global-mode 1)

;; Spellcheck for comments for every language.
(add-hook 'prog-mode-hook 'flyspell-prog-mode)

;; Haskell
(use-package haskell-mode)
(use-package lsp-haskell)

(dolist (mode '(haskell-indent-mode
                haskell-indentation-mode
                interactive-haskell-mode
                highlight-uses-mode
                haskell-decl-scan-mode
                (lambda ()
                  (setq-default haskell-indent-offset 2)
                  (setq-default haskell-indent-thenelse 2))))
  (add-hook 'haskell-mode-hook mode))

;; Agda
(load-file (let ((coding-system-for-read 'utf-8))
             (shell-command-to-string "agda-mode locate")))

;; Lisp dialects
;; This part needs more attention...
(use-package racket-mode)
(use-package scribble-mode)

;; Visit http://danmidwood.com/content/2014/11/21/animated-paredit.html for
;; a quick and complete tutorial.
(use-package paredit
  :ensure t
  :config
  (dolist (hook '(emacs-lisp-mode-hook
                  racket-mode
                  racket-repl-mode-hook))
    (add-hook hook 'paredit-mode)))

;; sh/bash
(setq-default sh-basic-offset 2)

;; Python
(use-package anaconda-mode)
(use-package company-anaconda)
(dolist
    (mode '(anaconda-mode
            anaconda-eldoc-mode))
  (add-hook 'python-mode-hook mode))
(eval-after-load "company"
  '(add-to-list 'company-backends 'company-anaconda))

;; TeX/LaTeX
;; (cfr. https://tex.stackexchange.com/a/50919)
(use-package auctex
  :ensure t)

(dolist
    (mode '(flyspell-mode
            LaTeX-math-mode
            ;; work with references
            turn-on-reftex
            ;; breaks lines when when wider than `fill-column`
            ;; combinations to remember:
            ;; * C-c C-q C-e
            ;; * C-c C-q C-s
            turn-on-auto-fill
            ;; sync with an external PDF reader
            TeX-source-correlate-mode
            ;; prettify (work in progress though...)
            prettify-symbols-mode
            (lambda ()
              (setq TeX-auto-save t)
              ;; parse *.tex files and get information that will be used
              ;; later by AUCTEX itself.
              (setq TeX-parse-self t)
              ;; decide which is the main file for every project
              (setq-default TeX-master nil)
              ;; things are supposed to end up some pdf
              (setq TeX-PDF-mode t)
              ;; set `fill-column` to a different value
              ;;(set-fill-column 72)  ;; the default is 70
              ;; folding
              (TeX-fold-mode nil)
              ;; convert tabs to spaces when *.tex files are saved
              (setq TeX-auto-untabify t)
              ;; prompt for the name of the TeX macro as you type \
              ;;(setq-local TeX-electric-escape t)
              ;; when you type $, \(...\) appears
              (setq TeX-electric-math '("\\(" . "\\)"))
              ;; automatically close braces
              (setq LaTeX-electric-left-right-brace t)
              ;; always put braces for sub- and superscripts
              ;;(setq TeX-electric-sub-and-superscript t)
              ;; raise or lower scripts to make life easier, I think
              ;;(setq font-latex-fontify-script nil)
              ;; add an empty pair of braces after macros without arguments
              (setq TeX-insert-braces t)
              ;; capture TeX errors
              (setq TeX-parse-all-errors t)
              ;; do not indent things within certain environments
              (add-to-list 'LaTeX-indent-environment-list
                           '("lstlisting" current-indentation))
              (add-to-list 'LaTeX-verbatim-environments "lstlisting")
              (add-to-list 'LaTeX-verbatim-macros-with-delims "lstinline"))))
  (add-hook 'LaTeX-mode-hook mode))

;; Markdown
(use-package markdown-mode
  :ensure t
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command
              '("pandoc" "--from=markdown" "--to=html5"))
  :bind (:map markdown-mode-map
              ("C-c C-e" . markdown-do)))

;; Org mode

;; General configuration
(setq org-pretty-entities t)
(setq org-pretty-entities-include-sub-superscripts t)

;; LaTeX integration
(dolist (mode '((lambda ()
                  ;; the compiler is lualatex, not the default (pdflatex)
                  (setq org-latex-compiler "lualatex")
                  ;; !!! edit `org-format-latex-header` as well...
                  ;; (setq org-format-latex-header nil)
                  ;; !!! modify directly this variable instead of
                  ;; !!! `org-latex-default-packages-alist`?
                  (setq org-latex-default-packages-alist
                        '(("AUTO" "inputenc" t ("pdflatex"))
                          ("T1" "fontenc" t ("pdflatex"))
                          ("no-math" "fontspec" t ("lualatex" "xelatex"))
                          ("AUTO" "babel" t ("pdflatex"))
                          ("AUTO" "polyglossia" t ("lualatex" "xelatex"))
                          ("" "microtype" nil nil)
                          ("rm,tt=false" "libertine" nil nil)
                          ("" "sourcecodepro" nil nil)
                          ("" "libertinust1math" t nil)
                          ("" "MnSymbol" t nil)
                          ("bb=stix" "mathalfa" t nil)
                          ("" "mathtools" t nil)
                          ("" "amsthm" nil nil)
                          ("" "tikz" t nil)
                          ("" "tikz-cd" t nil)
                          ("" "graphics" t nil)
                          ("" "longtable" nil nil)
                          ("" "wrapfig" nil nil)
                          ("" "rotating" nil nil)
                          ("normalem" "ulem" t nil)
                          ;; ("" "amsmath" t nil)
                          ;; ("" "amssymb" t nil)
                          ("" "capt-of" nil nil)
                          ("breaklinks,colorlinks" "hyperref" nil nil)
                          ("" "listings" nil nil)))
                  ;; use LaTeX package listings to render code blocks
                  (setq org-latex-src-block-backend "listings")
                  (setq org-latex-listings-options
                        '(("basicstyle"       "\\ttfamily")
                          ("commentstyle"     "\\color{gray!75!black}")
                          ("stringstyle"      "\\itshape")
                          ("showstringspaces" "false")
                          ("frame"            "leftline"))))
                ;; environments like in LaTeX
                turn-on-org-cdlatex
                ;; breaks lines when they are getting longer than the value
                ;; of `fill-columln`
                turn-on-auto-fill))
  (add-hook 'org-mode-hook mode))

;; Draw a vertical ruler on column 75 for some languages.
(use-package fill-column-indicator)
(setq fci-rule-column 75)
(dolist
    (hook '(sh-mode-hook
            perl-mode-hook
            python-mode-hook
            haskell-mode-hook
            scheme-mode-hook
            emacs-lisp-mode-hook
            racket-mode-hook
            org-mode-hook))
  (add-hook hook 'fci-mode))

;; @@@
