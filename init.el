;;; .emacs --- GNU Emacs configuration file -*- lexical-binding: t; -*-
;;;
;;; Commentary:
;;;
;;; A init file for Emacs.
;;; 
;;; Author: Indrjo Dedej
;;;
;;; Code:

;;; ********************************
;;; * Install & configure packages *
;;; ********************************

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

;; Make sure `use-package` is installed. It will be used to install
;; missing packages and configure them.
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)



;;; *************************
;;; * General configuration *
;;; *************************

;; Start with a blank screen. (Although, booting up with the GNU Emacs
;; welcome screen isn't bad at all...)
(setq inhibit-startup-message t)

;; Disable the menu bar. If disabled, press `Fn+F10` to make it pop up.
(menu-bar-mode -1)

;; Disable toolbar.
(tool-bar-mode -1)

;; Disable scroll bar.
(scroll-bar-mode -1)

;; Start scrolling if point is trying to go off screen vertically.
(setq scroll-margin 0)
;; Do not recenter point if you move off screen.
(setq scroll-conservatively 101)
;; Remember position if when scrolling point risks to fall off screen.
(setq scroll-preserve-screen-position t)
(setq auto-window-vscroll nil)

;; Disable tooltips.
(tooltip-mode -1)

;; On the mode-line will appear `(x, y)` indicating the position of
;; the cursor on the current window.
(column-number-mode 1)

;; Split window and immediately "C-x o". This is not the dafault, I
;; don't know why...
(defun split-horizontally-and-C-x-o ()
  (interactive)
  (split-window-below)
  (other-window 1))

(defun split-vertically-and-C-x-o ()
  (interactive)
  (split-window-right)
  (other-window 1))

(global-set-key (kbd "C-x 2") 'split-horizontally-and-C-x-o)
(global-set-key (kbd "C-x 3") 'split-vertically-and-C-x-o)

;; Disable auto-saving for every file you are visiting.
(setq auto-save-default nil)
;; Do not make backup files after the first save. Alternatively, see the
;; commented piece below.
(setq make-backup-files nil)

;; Make backup files in one precise directory. In any case, keep the
;; litter away from the working place.
;; (setq backup-dir (expand-file-name "~/.emacs.d/backup"))
;; (unless (file-exists-p backup-dir)
;;   (make-directory backup-dir t))
;; (setq backup-directory-alist
;;       `(("." . ,backup-dir)))

;; Needed for `shell-command-to-string` below. For reference see:
;; https://github.com/purcell/exec-path-from-shell
(use-package exec-path-from-shell)
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

;; Fonts
(setq main-font "Cascadia Mono")
(when (member main-font (font-family-list))
  (set-face-attribute 'default nil :family main-font))

;; The selected theme.
(use-package darcula-theme
  :config
  (load-theme 'darcula t))

;; (use-package all-the-icons
;;   :if (display-graphic-p))


;;; *************************
;;; * Programming & writing *
;;; *************************

;; Indentation
(setq-default indent-tabs-mode nil)

;; Pretty symbols
(setq prettify-symbols-unprettify-at-point 'right-edge)
(add-hook 'prog-mode-hook 'prettify-symbols-mode)
(add-hook 'text-mode-hook 'prettify-symbols-mode)

;; Spellcheck for comments and string constants.
(add-hook 'prog-mode-hook 'flyspell-prog-mode)
(add-hook 'text-mode-hook 'flyspell-mode)

;; Display line numbers on the right edge of the windows.
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
;;(add-hook 'text-mode-hook 'display-line-numbers-mode)

;; Highlight the current line.
(add-hook 'prog-mode-hook 'hl-line-mode)
(add-hook 'text-mode-hook 'hl-line-mode)

;; Auto-pairing quotes and parentheses
;; (add-hook 'prog-mode-hook 'electric-pair-mode)

;; Break comment lines as they become longer than the value set below.
(setq-default fill-column 72)
(add-hook 'prog-mode-hook 'display-fill-column-indicator-mode)

;; Visit also: https://emacs-lsp.github.io/lsp-mode/page/installation/.
(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-c l")
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

;; Check syntax
(use-package flycheck
  :config (global-flycheck-mode 1))

;; Completions
(use-package company
  :hook (prog-mode . company-mode))
;; (add-hook 'after-init-hook 'global-company-mode)

;; Snippets
(use-package yasnippet
  :config
  ;; Add the path of some personal snippets. My snippets will override
  ;; the others.
  (let ((my-snippet-dir (expand-file-name  "~/snippets/emacs/")))
    (add-to-list 'yas-snippet-dirs my-snippet-dir))
  (yas-global-mode 1))

;; A good collection of snippets for many languages, good to have.
(use-package yasnippet-snippets)

;; Haskell
(use-package haskell-mode
  :hook ((haskell-mode . haskell-indent-mode)
	 (haskell-mode . haskell-indentation-mode)
	 (haskell-mode . interactive-haskell-mode)
	 (haskell-mode . haskell-indent-mode)
	 (haskell-mode . highlight-uses-mode)
	 (haskell-mode . haskell-decl-scan-mode)
         (haskell-mode . electric-pair-local-mode))
  :custom
  (haskell-indent-offset 2)
  (haskell-indent-thenelse 2))

(use-package lsp-haskell)

;; Agda
(let ((coding-system-for-read 'utf-8)
      (agda-mode-path (shell-command-to-string "agda-mode locate")))
  (load-file agda-mode-path))

;; Lisp dialects. (This part needs more attention...)
(use-package racket-mode
  :custom
  ;; Set a memory limit (in MB) for computations.
  (racket-memory-limit 128))

(use-package scribble-mode)

(use-package paren-face
  :hook ((emacs-lisp-mode . paren-face-mode)
	 (racket-mode . paren-face-mode)
	 (racket-repl-mode . paren-face-mode)))

;; http://danmidwood.com/content/2014/11/21/animated-paredit.html
(use-package paredit
  :hook ((emacs-lisp-mode . paredit-mode)
	 (racket-mode . paredit-mode)))

;; Python
(use-package anaconda-mode
  :hook ((python-mode . anaconda-mode)
	 (python-mode . anaconda-eldoc-mode)
         (python-mode . electric-pair-local-mode)))

(use-package company-anaconda
  :config
  (add-to-list 'company-backends 'company-anaconda))

;; TeX/LaTeX
;; (cfr. https://tex.stackexchange.com/a/50919)
(use-package auctex
  :defer t)

(dolist
    (mode '(;; TeX-fold-mode
            LaTeX-math-mode
            ;; Work with references
            turn-on-reftex
            ;; Breaks lines when when wider than `fill-column`
            ;; Combinations to remember:
            ;; * C-c C-q C-e
            ;; * C-c C-q C-s
            turn-on-auto-fill
            ;; Sync with an external PDF reader
            TeX-source-correlate-mode
            ;; Line numbers on the left side
            display-line-numbers-mode
	    (lambda ()
              ;; (add-hook 'find-file-hook 'TeX-fold-buffer t)
              ;; (setq TeX-fold-auto t)
              ;; (setq TeX-fold-preserve-comments t)
              (setq TeX-auto-save t)
              ;; Parse *.tex files and get information that will be
              ;; used Later by AUCTEX itself.
              (setq TeX-parse-self t)
              ;; Decide which is the main file for every project
              (setq TeX-master nil)
              ;; Things are supposed to end up with some pdf
              (setq TeX-PDF-mode t)
              ;; Convert tabs to spaces when *.tex files are saved
              (setq TeX-auto-untabify t)
              ;; When you type $, \(...\) appears
              (setq TeX-electric-math '("\\(" . "\\)"))
              ;; Automatically close braces
              (setq LaTeX-electric-left-right-brace t)
              ;; Capture TeX errors
              (setq TeX-parse-all-errors t)
              ;; Integrate RefTeX into AUCTeX.
              (setq reftex-plug-into-AUCTeX t)
	      ;; Listings for code snippets
	      (setq org-latex-src-block-backend 'listings)
              ;; Do not indent things within certain environments
              (add-to-list 'LaTeX-indent-environment-list
                           '("lstlisting" current-indentation))
              (add-to-list 'LaTeX-verbatim-environments "lstlisting")
              (add-to-list 'LaTeX-verbatim-macros-with-delims "lstinline"))))
  (add-hook 'LaTeX-mode-hook mode))



;; Org mode

(use-package olivetti)
(use-package cdlatex)

;; see: https://orgmode.org/manual/Conflicts.html
(defun yas/org-very-safe-expand ()
  (let ((yas/fallback-behavior 'return-nil)) (yas/expand)))

(use-package org
  :hook ((org-mode . visual-line-mode)
         (org-mode . org-indent-mode)
	 (org-mode . turn-on-org-cdlatex)
	 (org-mode . turn-on-auto-fill)
         (org-mode . olivetti-mode)
         (org-mode . (lambda ()
                       (make-variable-buffer-local 'yas/trigger-key)
                       (setq yas/trigger-key [tab])
                       (add-to-list 'org-tab-first-hook 'yas/org-very-safe-expand)
                       (define-key yas/keymap [tab] 'yas-next-field))))
  :custom
  ;; General configuration.
  (org-pretty-entities t)
  (org-adapt-indentation t)
  (org-hide-leading-starts t)
  (org-hide-emphasis-markers t)
  (org-ellipsis " [...]")
  (org-pretty-entities-include-sub-superscripts t)
  ;; Use org-mode to produce pdfs with LaTeX.
  (org-latex-compiler "lualatex")
  (org-latex-default-packages-alist
   '(("AUTO" "inputenc" t ("pdflatex"))
     ("T1" "fontenc" t ("pdflatex"))
     ("" "babel" nil ("pdflatex"))
     ("no-math" "fontspec" t ("lualatex" "xetex"))
     ("" "polyglossia" nil ("lualatex" "xelatex"))
     ("" "microtype" nil nil)
     ("" "graphicx" t nil)
     ("" "longtable" nil nil)
     ("" "wrapfig" nil nil)
     ("" "rotating" nil nil)
     ("normalem" "ulem" t nil)
     ("" "capt-of" nil nil)
     ("breaklinks,colorlinks" "hyperref" nil nil)
     ("rm,tt=false" "libertine" t nil)
     ("" "libertinust1math" t nil)
     ("" "MnSymbol" t nil)
     ("bb=stix" "mathalfa" t nil)
     ("" "mathtools" t nil)
     ("" "tikz" t nil)
     ("" "tikz-cd" t nil)
     ("" "listings" t nil)))
  (org-latex-src-block-backend 'listings)
  (org-latex-listings-options
   '(("basicstyle"       "\\small\\ttfamily")
     ("commentstyle"     "\\color{gray!75!black}")
     ("stringstyle"      "\\itshape")
     ("showstringspaces" "false")
     ("frame"            "leftline"))))

(use-package org-modern
  :hook (org-mode . org-modern-mode))

;; Markdown
(use-package markdown-mode
  :mode ("\\.md\\'" . gfm-mode)
  :custom
  (markdown-command "pandoc -f markdown -t html --standalone"))

;;; .emacs ends here

