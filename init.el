;; Bootstrap straight package manager. The straight
;; package manager usually works better if I'm setting
;; up emacs on a new machine :)
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

;; use setq-default to set it for /all/ modes
(setq-default mode-line-format
              (list
               "λ> "

               ;; the buffer name; the file name as a tool tip
               '(:eval (propertize "%b " 'face 'font-lock-keyword-face
                                   'help-echo (buffer-file-name)))

               ;; line and column
               "(" ;; '%02' to set to 2 chars at least; prevents flickering
               (propertize "%02l" 'face 'font-lock-type-face) ","
               (propertize "%02c" 'face 'font-lock-type-face)
               ") "

               ;; relative position, size of file
               "["
               (propertize "%p" 'face 'font-lock-constant-face) ;; % above top
               "] "

               ;; the current major mode for the buffer.
               "["

               '(:eval (propertize "%m" 'face 'font-lock-string-face
                                   'help-echo buffer-file-coding-system))
               "] "


               "[" ;; insert vs overwrite mode, input-method in a tooltip
               '(:eval (propertize (if overwrite-mode "Ovr" "Ins")
                                   'face 'font-lock-preprocessor-face
                                   'help-echo (concat "Buffer is in "
                                                      (if overwrite-mode "overwrite" "insert") " mode")))

               ;; was this buffer modified since the last save?
               '(:eval (when (buffer-modified-p)
                         (concat ","  (propertize "Mod"
                                                  'face 'font-lock-warning-face
                                                  'help-echo "Buffer has been modified"))))

               ;; is this buffer read-only?
               '(:eval (when buffer-read-only
                         (concat ","  (propertize "RO"
                                                  'face 'font-lock-type-face
                                                  'help-echo "Buffer is read-only"))))
               "] "

               ;; add the time, with the date and the emacs uptime in the tooltip
               '(:eval (propertize (format-time-string "%H:%M")
                                   'help-echo
                                   (concat (format-time-string "%c; ")
                                           (emacs-uptime "Uptime:%hh"))))
               ))


;; colorize output in compile buffer
(require 'ansi-color)
(defun colorize-compilation-buffer ()
  (ansi-color-apply-on-region compilation-filter-start (point-max)))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

(use-package modus-themes
  :straight t
  :config
  (setq modus-themes-common-palette-overrides
        '((border-mode-line-active unspecified)
          (border-mode-line-inactive unspecified)))

  (setq modus-vivendi-tinted-palette-overrides
        '((bg-main "#181818")))

  (load-theme 'modus-vivendi-tinted t))

(use-package rust-mode
  :straight t)

(use-package embark
  :straight t

  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)  ;; alternative for `describe-bindings'
   ("C-c C-e" . embark-export))

  :init

  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  ;; Show the Embark target at point via Eldoc. You may adjust the
  ;; Eldoc strategy, if you want to see the documentation from
  ;; multiple providers. Beware that using this can be a little
                                        ;>; jarring since the message shown in the minibuffer can be more
  ;; than one line, causing the modeline to move up and down:

  ;; (add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target)
  ;; (setq eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)

  :config

  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

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

;; Example configuration for Consult
(use-package consult
  :straight t
  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :bind
  (("M-s f" . consult-fd)
   ("M-s r" . consult-ripgrep))
  :hook (completion-list-mode . consult-preview-at-point-mode))

;; Does straight not work with this?
;; Do I need to include the separate repo?
(use-package embark-consult
  :straight
  (:host github :repo "emacs-straight/embark-consult")
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package wgrep
  :straight t)

(use-package multiple-cursors
  :straight t
  :bind
  ("C->" . mc/mark-next-like-this)
  ("C-<" . mc/mark-previous-like-this)
  ("C-c C->" . mc/mark-all-like-this)
  ("C-M->" . mc/skip-to-next-like-this))

;; Please no type hints

(setopt eglot-ignored-server-capabilities '(:inlayHintProvider))

(with-eval-after-load "eglot"
  (add-to-list 'eglot-stay-out-of 'flymake)
  (add-to-list 'eglot-stay-out-of 'flycheck))
