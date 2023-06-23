;; Sources that made possible this configuration file:
;;
;; >>> https://github.com/daviwil/emacs-from-scratch
;; >>> https://www.youtube.com/watch?v=74zOY-vgkyw

;; Show the current column number too.
(setq column-number-mode t)

;; Insert spaces when you hit tab.
(setq-default indent-tabs-mode nil)

;; Uncluttering my workspace a little bit...
;(setq inhibit-startup-message t)
(tool-bar-mode -1)
(tooltip-mode -1)
(set-fringe-mode 10)

;; Add packages archives, since it appears to be empty by default.
(require 'package)
(setq package-archives
      '(("elpa"   . "https://elpa.gnu.org/packages/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")
        ("melpa"  . "https://melpa.org/packages/")
        ("org"    . "https://orgmode.org/elpa/")))
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Make sure `use-package` is installed. It will be used to install missing
;; packages and configure them.
(unless (package-installed-p 'use-package)
   (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)

;; Show event or command history of certain buffers.
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

;; A library to make sure the environment variables inside GNU Emacs are
;; the same of your shell. For future reference see:
;; >>> https://github.com/purcell/exec-path-from-shell#usage
(use-package exec-path-from-shell)
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

;; Haskell
(use-package haskell-mode)

;; Agda
(load-file (let ((coding-system-for-read 'utf-8))
   (shell-command-to-string "agda-mode locate")))

;; TeX
(use-package tex
  :ensure auctex)

;; Git
(use-package magit)

;; The selected theme.
(use-package darcula-theme
  :ensure t)
(load-theme 'darcula t)

;; A directory to be used as a sandbox and not to mess the GNU Emacs up.
(setq custom-dir "~/.emacs.d/custom/")
(unless (file-exists-p custom-dir)
  (make-directory custom-dir))
(add-to-list 'load-path custom-dir)

;; Draw a vertical ruler on column 75. For future reference:
;; >>> https://www.emacswiki.org/emacs/FillColumnIndicator
;; !!! You have to download by yourself the file of the link above into the
;; !!! directory ~/.emacs.d/custom.
(when (file-exists-p
       (concat custom-dir "fill-column-indicator.el"))
  (require 'fill-column-indicator)
  (setq fci-rule-column 75)
  ;; The vertical rule is only for the languages I specifiy below...
  (add-hook 'haskell-mode-hook #'fci-mode)
  (add-hook 'scheme-mode-hook #'fci-mode)
  (add-hook 'emacs-lisp-mode-hook #'fci-mode)
  (add-hook 'sh-mode-hook #'fci-mode)
  (add-hook 'perl-mode-hook #'fci-mode)
  (add-hook 'python-mode-hook #'fci-mode))

;; TAB is 2 spaces for sh scripts
(setq-default sh-basic-offset 2)

;; Spellcheck for all the programming languages
(add-hook 'prog-mode-hook #'flyspell-prog-mode)

;; @@@
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(auctex-cluttex magit auctex-lua auctex-latexmk auctex zenburn-theme use-package jetbrains-darcula-theme ivy haskell-mode flatland-theme exec-path-from-shell darcula-theme compat command-log-mode)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
