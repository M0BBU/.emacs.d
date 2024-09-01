;; -*- mode: emacs-lisp; lexical-binding: t -*-

;; Bootstrap straight package manager. The straight
;; package manager usually works better if I'm setting
;; up emacs on a new machine :)
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
	     "https://radian-software.github.io/straight.el/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Line numbers
(global-display-line-numbers-mode)
;; Ensure files are always updated after git
(global-auto-revert-mode t)

;; Get rid of top ui for more screen space.
(tooltip-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq indent-line-function 'insert-tab)

;; Smooth scrolling
(pixel-scroll-precision-mode)

;; Sets M-o to switch windows
(global-set-key "\M-o" 'other-window)

(column-number-mode)
(display-time-mode)

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
 mac-option-modifier 'super)

;; Maximize window on startup!
(add-hook 'window-setup-hook 'toggle-frame-maximized t)

;; Lets you highlight and delete
(delete-selection-mode t)

;; Accept `y` or `n` instead of yes or no.
(defalias 'yes-or-no-p 'y-or-n-p)

;; Don't mix tabs and spaces! The setq-default is needed because this becomes
;; a buffer-local variable when set (I believe).
(setq-default indent-tabs-mode nil)
;; I like 4-char indents :]
(setq-default tab-width 4)

;; Scrolls one line without moving cursor
(global-set-key (kbd "M-n") #'scroll-up-line)
(global-set-key (kbd "M-p") #'scroll-down-line)

;; Get rid of trailing whitespace
(add-hook 'before-save-hook #'delete-trailing-whitespace)
(setq require-final-newline t)

;; Ido and fido-vertical is pretty nice
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)
(fido-vertical-mode t)

;; Control font size?
(set-face-attribute 'default nil :height 170)

;; Get rid of backups and autosaves
(setq
 make-backup-files nil
 auto-save-default nil
 create-lockfiles nil)

;; Let straight be the package manager for use-package :]
(straight-use-package 'use-package)

;; colorize output in compile buffer
(require 'ansi-color)
(defun colorize-compilation-buffer ()
  (ansi-color-apply-on-region compilation-filter-start (point-max)))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

;; Custom Theme :]
(use-package modus-themes
  :straight t
  :config
  (setq modus-themes-common-palette-overrides
        '((border-mode-line-active unspecified)
          (border-mode-line-inactive unspecified)))

  (setq modus-vivendi-tinted-palette-overrides
        '((bg-main "#181818")
          (fringe "#181818")
          (bg-line-number-inactive unspecified "#181818")))

  (load-theme 'modus-vivendi-tinted t))

;; Enable rich annotations using the Marginalia package
(use-package marginalia
  :straight t
  ;; Bind `marginalia-cycle' locally in the minibuffer.  To make the binding
  ;; available in the *Completions* buffer, add it to the
  ;; `completion-list-mode-map'.
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle))

  ;; The :init section is always executed.
  :init

  ;; Marginalia must be activated in the :init section of use-package such that
  ;; the mode gets enabled right away. Note that this forces loading the
  ;; package.
  (marginalia-mode))

(use-package orderless
  :straight t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package consult
  :straight t
  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :bind
  (("M-s r" . consult-ripgrep)
   ("M-s f" . consult-fd)
   ("M-s e" . consult-flymake)
   ("C-x C-b" . consult-buffer))
  :hook (completion-list-mode . consult-preview-at-point-mode))

(use-package corfu
  :straight t
  ;; Optional customizations
  :custom
  (corfu-cycle t)               ;; Enable cycling for `corfu-next/previous'
  ;; (corfu-separator ?\s)          ;; Orderless field separator
  ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  ;; (corfu-preview-current nil)    ;; Disable current candidate preview
  ;; (corfu-preselect 'prompt)      ;; Preselect the prompt
  ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches
  ;; (corfu-scroll-margin 5)        ;; Use scroll margin

  ;; Enable Corfu only for certain modes. See also `global-corfu-modes'.
  ;; :hook ((prog-mode . corfu-mode)
  ;;        (shell-mode . corfu-mode)
  ;;        (eshell-mode . corfu-mode))

  ;; Recommended: Enable Corfu globally.  This is recommended since Dabbrev can
  ;; be used globally (M-/).  See also the customization variable
  ;; `global-corfu-modes' to exclude certain modes.
  :bind
  (:map corfu-map
        ("TAB" . corfu-next)
        ([tab] . corfu-next)
        ("S-TAB" . corfu-previous)
        ([backtab] . corfu-previous))
  :init
  (global-corfu-mode))


(use-package magit
  :straight t
  :config
  (remove-hook 'magit-status-sections-hook 'magit-insert-tags-header)
  (remove-hook 'magit-status-sections-hook 'magit-insert-status-headers)
;;  (remove-hook 'magit-status-sections-hook 'magit-insert-unpushed-to-pushremote)
;;  (remove-hook 'magit-status-sections-hook 'magit-insert-unpulled-from-pushremote)
  (remove-hook 'magit-status-sections-hook 'magit-insert-unpulled-from-upstream)
  (remove-hook 'magit-status-sections-hook 'magit-insert-unpushed-to-upstream-or-recent))

(use-package direnv
  :straight t
  :config
  (direnv-mode))

(use-package flymake
  :config ; (Optional) For fix bad icon display (Only for left margin)
  (advice-add #'flymake--indicator-overlay-spec
              :filter-return
              (lambda (indicator)
                (concat indicator
                        (propertize " "
                                    'face 'default
                                    'display `((margin left-margin)
                                               (space :width 5))))))
  :custom
  (flymake-indicator-type 'margins)
  (flymake-margin-indicators-string
   `((error ,(propertize "•") compilation-error)
     (warning ,(propertize "•") compilation-warning)
     (note ,(propertize "•") compilation-info))))


;;;###autoload
(add-to-list 'auto-mode-alist (cons "\\.go\\'" 'go-ts-mode))
(add-hook 'go-ts-mode (lambda () (electric-indent-local-mode -1)))
(setq go-ts-mode-indent-offset 4)

;; Need to tell emacs where to get the grammars
(setq treesit-language-source-alist
      '((go "https://github.com/tree-sitter/tree-sitter-go" "v0.20.0")
        (starlark "https://github.com/tree-sitter-grammars/tree-sitter-starlark.git" "v1.1.0")
        (bash "https://github.com/tree-sitter/tree-sitter-bash")
        (cmake "https://github.com/uyha/tree-sitter-cmake")
        (elisp "https://github.com/Wilfred/tree-sitter-elisp")
        (json "https://github.com/tree-sitter/tree-sitter-json")
        (make "https://github.com/alemuller/tree-sitter-make")
        (markdown "https://github.com/ikatyang/tree-sitter-markdown")
        (toml "https://github.com/tree-sitter/tree-sitter-toml")
        (yaml "https://github.com/ikatyang/tree-sitter-yaml")))

;; Setup LSP and Treesitter
(use-package eglot
  :init
  (add-hook 'go-ts-mode-hook 'eglot-ensure)
  (add-hook 'go-mode-hook 'eglot-ensure)
  :config
  (add-to-list 'eglot-ignored-server-capabilites
               ':documentHighlightProvider
               ':inlayHintProvider))

(with-eval-after-load "eglot"
  (add-to-list 'eglot-stay-out-of 'flycheck))
