
;; Uncluttering my workspace a little bit...
(tool-bar-mode -1)     ; no toolbar
(tooltip-mode -1)      ; no tooltips
(set-fringe-mode 0)    ; get rid of fringes

;; Add packages archives, since it appears to be empty by deafault.
(require 'package)
(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
        ("org" . "https://orgmode.org/elpa/")
        ("elpa" . "https://elpa.gnu.org/packages/")))
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
;; a vast number of choices in certain buffers.
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

;; A library to make sure the the environment variables inside GNU Emacs
;; are the same of your shell. For future reference see:
;; >>> https://github.com/purcell/exec-path-from-shell#usage
(use-package exec-path-from-shell)
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

;; Haskell syntax highlight
(use-package haskell-mode)

;; Agda
(load-file (let ((coding-system-for-read 'utf-8))
   (shell-command-to-string "agda-mode locate")))

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
  (add-hook 'prog-mode-hook #'fci-mode))

;; @@@
