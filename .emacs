;; Sources that made possible this configuration file:
;;
;; >>> https://github.com/daviwil/emacs-from-scratch
;; >>> https://www.youtube.com/watch?v=74zOY-vgkyw

;; Show the current column number too.
(setq-default column-number-mode t)

;; Insert spaces when you hit tab.
(setq-default indent-tabs-mode nil)

;; Uncluttering my workspace a little bit...
(setq inhibit-startup-message t)
(tool-bar-mode -1)
(tooltip-mode -1)
(set-fringe-mode 6)

;; Set up the visible bell
(setq visible-bell t)

;; Show line numbers
(column-number-mode)
(global-display-line-numbers-mode t)
(dolist
    (mode '(org-mode-hook
            term-mode-hook
            shell-mode-hook
            treemacs-mode-hook
            eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

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

;; Keep clean.
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
  :config
  (ivy-mode 1))

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

;; Haskell
(use-package haskell-mode)

;; Agda
(load-file (let ((coding-system-for-read 'utf-8))
             (shell-command-to-string "agda-mode locate")))

;; Lisp dialects
;;(use-package racket-mode)
(use-package geiser-racket)
(use-package geiser-guile)
(use-package paredit)
(autoload 'enable-paredit-mode
  "paredit" "Turn on pseudo-structural editing of Lisp code." t)
(add-hook 'emacs-lisp-mode-hook #'enable-paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
(add-hook 'ielm-mode-hook #'enable-paredit-mode)
(add-hook 'lisp-mode-hook #'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
(add-hook 'scheme-mode-hook #'enable-paredit-mode)

;; sh/bash
(setq-default sh-basic-offset 2)

;; TeX
(use-package tex
  :ensure auctex)
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)

(use-package latex-preview-pane)
(latex-preview-pane-enable)

;; Markdown
(use-package markdown-mode
  :ensure t
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command
              '("pandoc" "--from=markdown" "--to=html5"))
  :bind (:map markdown-mode-map
              ("C-c C-e" . markdown-do)))

;; Git
(use-package magit)

;; The selected theme.
(use-package darcula-theme
  :ensure t)
(load-theme 'darcula t)

;; Draw a vertical ruler on column 75.
(use-package fill-column-indicator)
(setq fci-rule-column 75)
;; The vertical rule is only for the languages I indicate below...
(add-hook 'haskell-mode-hook #'fci-mode)
(add-hook 'scheme-mode-hook #'fci-mode)
(add-hook 'emacs-lisp-mode-hook #'fci-mode)
(add-hook 'sh-mode-hook #'fci-mode)
(add-hook 'perl-mode-hook #'fci-mode)
(add-hook 'python-mode-hook #'fci-mode)

;; Spellcheck for all the programming languages
(add-hook 'prog-mode-hook #'flyspell-prog-mode)

;; A directory to be used as a sandbox and not to mess the GNU Emacs up.
(setq custom-dir "~/.emacs.d/custom/")
(unless (file-exists-p custom-dir)
  (make-directory custom-dir))
(add-to-list 'load-path custom-dir)

;; @@@
