;; -*- coding: utf-8; lexical-binding: t -*-

;; Bootstrap straight package manager. I find that straight usually
;; works better if I need to setup emacs of a different machine.
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))


;; Get rid of top ui for more screen space.
(tooltip-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

;; Smooth scrolling
(pixel-scroll-precision-mode)

;; Nice colorscheme
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(load-theme 'zenburn t)

;; Some nice defaults
(setq
 ;; No GNU message.
 inhibit-startup-message t
 ;; No welcome message.
 inhibit-splash-screen t
 ;; No reminder for scratch buffer.
 initial-scratch-message nil
 ;; No dings!
 ring-bell-function 'ignore
 ;; Let C-k delete the whole line.
 kill-whole-line t
 ;; Search is case sensitive.
 case-fold-search nil
 ;; I dunno how to fix the warnings with native-comp :]
 native-comp-async-report-warnings-errors 'silent
 ;; Command == Meta to save my fingers.
 mac-command-modifier 'meta
 ;; Make opt key do Super.
 mac-option-modifier 'super
 )

;; Highlight the current line I'm on
(global-hl-line-mode 1)

;; Maximize window on startup!
(add-hook 'window-setup-hook 'toggle-frame-maximized t)

;; Accept `y` or `n` instead of yes or no.
(defalias 'yes-or-no-p 'y-or-n-p)

;; Don't mix tabs and spaces! The setq-default is needed because this becomes
;; a buffer-local variable when set (I believe).
(setq-default indent-tabs-mode nil)
;; I like 4-char indents :]
(setq-default tab-width 4)

;; Sets M-o to switch windows
(global-set-key "\M-o" 'other-window)

;; Turn on some modes for modern niceties
(delete-selection-mode t)
(global-display-line-numbers-mode t)
(column-number-mode)

;; Scrolls one line without moving cursor
(global-set-key (kbd "M-n") #'scroll-up-line)
(global-set-key (kbd "M-p") #'scroll-down-line)

;; 80 column rule
(setq-default fill-column 80)
(global-display-fill-column-indicator-mode t)

;; Nice font
(set-face-attribute 'default nil :font "Iosevka 17")

;; Get rid of backups and autosaves
(setq
 make-backup-files nil
 auto-save-default nil
 create-lockfiles nil
 )

;; Get rid of trailing whitespace
(add-hook 'before-save-hook #'delete-trailing-whitespace)
(setq require-final-newline t)

;; Ido and fido-vertical is pretty nice
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)
(fido-vertical-mode t)

;; Let straight be the package manager for use-package :]
(straight-use-package 'use-package)

;; All my packages and configurations!
(use-package magit
  :straight t
  :diminish magit-auto-revert-mode
  :diminish auto-revert-mode)

(use-package go-mode
  :straight t)

(use-package rust-mode
  :straight t
  :mode ("\\.rs\\'" . rust-mode)
  :config
  (add-hook 'rust-mode-hook
            (lambda () (setq indent-tabs-mode nil))))

(use-package dumb-jump
  :straight t)
(add-hook 'xref-backend-functions #'dumb-jump-xref-activate)

(use-package which-key
  :straight t
  :config
  (which-key-mode))

(use-package pdf-tools
  :straight t)


(use-package tex
  :straight auctex
  :config
  (setq TeX-electric-sub-and-superscript t)
  (setq TeX-electric-math (cons "$" "$"))
  ;; Let PDF tools be our default viewwer
  (setq TeX-view-program-selection '((output-pdf "PDF Tools"))
        TeX-source-correlate-start-server t)
  ;; This makes PDF tools refresh after compilation :)
  (add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer)
  ;; Automatically close pairs for me
  (add-hook 'LaTeX-mode-hook #'electric-pair-mode)
  ;; Automatically break lines at the 80 char limit
  (add-hook 'LaTeX-mode-hook #'auto-fill-mode)
  (add-hook 'LaTeX-mode-hook #'flyspell-mode))

(use-package corfu
  :straight t
  :custom
  (corfu-auto t)
  :config
  (setq-local corfu-auto-delay 0.75
              completion-styles '(basic))
  :bind
  (:map corfu-map
        ("TAB" . corfu-next)
        ([tab] . corfu-next)
        ("S-TAB" . corfu-previous)
        ([backtab] . corfu-previous))
  :init)

(use-package consult
  :straight t)

(use-package popper
  :straight t
  :bind (("C-`"   . popper-toggle)
         ("M-`"   . popper-cycle))
  :init
  (setq popper-reference-buffers
        '("\\*Messages\\*"
          "Output\\*$"
          "\\*Async Shell Command\\*"
          help-mode
          compilation-mode))
  (popper-mode +1)
  (popper-echo-mode +1))

(use-package org
  :straight t
  :config
  (setq org-log-done t)
  (setq org-agenda-files (list "~/org/school/todo.org")))

(use-package haskell-mode
  :straight t)

(use-package eglot
  :config
  ;; Inlay hints make me lose my mind with rust analyzer
  (setq-default eglot-inlay-hints-mode nil))

;; cc-mode defaults
(setq c-default-style "stroustrup")
(add-hook 'c++-mode-hook (lambda nil (setq c-basic-offset 4)))

;; cmake-mode :]
(setq load-path (cons (expand-file-name "/opt/homebrew/Cellar/cmake/3.27.4/share/emacs/site-lisp/cmake") load-path))
(require 'cmake-mode)
