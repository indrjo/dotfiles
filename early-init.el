;; -*- lexical-binding: t; -*-

;; Make startup faster by reducing the frequency of garbage
;; collection. This will be set back when startup finishes. We also
;; increase Read Process Output Max so Emacs can read more data.

(setq gc-cons-threshold (* 1024 1024 128))
(setq gc-cons-percentage 1.0)

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 1024 1024 2))
            (setq  gc-cons-percentage 0.2)))

;; Increase the amount of data which Emacs reads from the process.
(setq read-process-output-max (* 1024 1024))

;; Unset file-name-handler-alist
(defvar last-file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)
(add-hook 'after-init-hook
          (lambda ()
            (setq file-name-handler-alist last-file-name-handler-alist)))

;; Disable toolbars, menus, and other visual elements for faster
;; startup:
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(tooltip-mode -1)
(setq inhibit-startup-screen t)

(prefer-coding-system 'utf-8)

;; Start scrolling if point is trying to go off screen vertically.
(setq scroll-margin 0)
;; Do not recenter point if you move off screen.
(setq scroll-conservatively 101)
;; Remember position if when scrolling point risks to fall off screen.
(setq scroll-preserve-screen-position t)
(setq auto-window-vscroll nil)

;; On the mode-line will appear `(x, y)` indicating the position of
;; the cursor on the current window.
(column-number-mode 1)

;; Disable auto-saving for every file you are visiting.
(setq auto-save-default nil)
;; Do not make backup files after the first save. Alternatively, see the
;; commented piece below.
(setq make-backup-files nil)

;; Fonts
(setq main-font "Cascadia Mono")
(when (member main-font (font-family-list))
  (set-face-attribute 'default nil :family main-font))

;; ;; Check if init.el exists, if not, tangle init.org to produce init.el
;; (let ((init-el (expand-file-name "init.el" user-emacs-directory))
;;       (init-org (expand-file-name "init.org" user-emacs-directory)))
;;   (unless (file-exists-p init-el)
;;     (when (file-exists-p init-org)
;;       (require 'org)
;;       (org-babel-tangle-file init-org init-el)
;;       (message "Tangling %s to create missing %s" init-org init-el))))
