;; -*- mode: emacs-lisp; lexical-binding: t -*-

;; Install use-package support.
(elpaca elpaca-use-package
  ;; Enable use-package :ensure support for Elpaca.
  (elpaca-use-package-mode))

(use-package emacs
  :config
  ;; Some nice defaults.
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
   ;; Sets CMD to Meta on Mac. Saves my fingers.
   mac-command-modifier 'meta)

  ;; 80 character line limit
  (setopt display-fill-column-indicator-column 80)
  (global-display-fill-column-indicator-mode)

  ;; Line numbers
  (global-display-line-numbers-mode)
  ;; Ensure files are always updated after git
  (global-auto-revert-mode t)

  ;; Get rid of top ui for more screen space.
  (tooltip-mode -1)
  (tool-bar-mode -1)
  (menu-bar-mode -1)
  (scroll-bar-mode -1)

  ;; Default indentation.
  (setq-default indent-tabs-mode nil)
  (setq-default tab-width 4)
  (setq indent-line-function 'insert-tab)
  ;; Don't mix tabs and spaces. The setq-default is needed because this becomes
  ;; a buffer-local variable when set (I believe).
  (setq-default indent-tabs-mode nil)
  ;; I like 4-char indents :)
  (setq-default tab-width 4)

  ;; Smooth scrolling.
  (pixel-scroll-precision-mode)

  ;; Sets M-o to switch windows.
  (global-set-key "\M-o" 'other-window)

  ;; Mode line luxuries.
  (column-number-mode)
  (display-time-mode)

  ;; Maximize window on startup.
  (add-hook 'window-setup-hook 'toggle-frame-maximized t)

  ;; Lets you highlight and delete.
  (delete-selection-mode t)

  ;; Accept `y` or `n` instead of yes or no.
  (defalias 'yes-or-no-p 'y-or-n-p)

  ;; Scrolls one line without moving cursor.
  (global-set-key (kbd "M-n") #'scroll-up-line)
  (global-set-key (kbd "M-p") #'scroll-down-line)
  (keymap-global-set "C-c C-c" #'compile)

  (setq compilation-scroll-output t)

  ;; Proper shell when executing commands with either shell-command or compile.
  (setq shell-file-name "zsh")
  (setq shell-command-switch "-ic")

  ;; Get rid of trailing whitespace.
  (add-hook 'before-save-hook #'delete-trailing-whitespace)
  (setq require-final-newline t)

  ;; Control font size?
  (set-face-attribute 'default nil :height 180)
  (set-frame-font "Comic Mono" nil t)

  ;; Get rid of backups and autosaves.
  (setq
   make-backup-files nil
   auto-save-default nil
   create-lockfiles nil)

;;   ;; Custom theme.
  (setq modus-themes-common-palette-overrides
        '((border-mode-line-active unspecified)
          (border-mode-line-inactive unspecified)))

  (setq modus-vivendi-tinted-palette-overrides
        '((bg-main "#181818")
          (fringe "#181818")
          (bg-line-number-inactive unspecified "#181818")))
  (load-theme 'modus-vivendi-tinted t))

(use-package ansi-color
  :config
  (add-hook 'compilation-filter-hook #'ansi-color-compilation-filter))

;; Enable Vertico.
(use-package vertico
  :ensure
  ;; custom:
  ;; (vertico-scroll-margin 0) ;; Different scroll margin
  ;; (vertico-count 20) ;; Show more candidates
  ;; (vertico-resize t) ;; Grow and shrink the Vertico minibuffer
  ;; (vertico-cycle t) ;; Enable cycling for `vertico-next/previous'
  :init
  (vertico-mode))

;; Configure directory extension.
(use-package vertico-directory
  :after vertico
  :ensure nil
  ;; More convenient directory navigation commands
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  ;; Tidy shadowed file names
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

(use-package orderless
  :ensure
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

;; Example configuration for Consult
(use-package consult
  :ensure
  ;; Replace bindings. Lazily loaded by `use-package'.
  :bind (;; C-c bindings in `mode-specific-map'
         ("C-c M-x" . consult-mode-command)
         ("C-c h" . consult-history)
         ("C-c k" . consult-kmacro)
         ("C-c m" . consult-man)
         ("C-c i" . consult-info)
         ([remap Info-search] . consult-info)
         ;; C-x bindings in `ctl-x-map'
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x t b" . consult-buffer-other-tab)    ;; orig. switch-to-buffer-other-tab
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ;; M-g bindings in `goto-map'
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings in `search-map'
         ("M-s f" . consult-fd)                  ;; Alternative: consult-fd
         ("M-s c" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history))                ;; orig. previous-matching-history-element

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (Not lazy)
  :init

  ;; Tweak the register preview for `consult-register-load',
  ;; `consult-register-store' and the built-in commands.  This improves the
  ;; register formatting, adds thin separator lines, register sorting and hides
  ;; the window mode line.
  (advice-add #'register-preview :override #'consult-register-window)
  (setq register-preview-delay 0.5)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key "M-.")
  ;; (setq consult-preview-key '("S-<down>" "S-<up>"))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep consult-man
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; "C-+"

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (keymap-set consult-narrow-map (concat consult-narrow-key " ?") #'consult-narrow-help)
  )

(use-package embark
  :ensure t

  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("C-;" . embark-export)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'

  :init

  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  ;; Show the Embark target at point via Eldoc. You may adjust the
  ;; Eldoc strategy, if you want to see the documentation from
  ;; multiple providers. Beware that using this can be a little
  ;; jarring since the message shown in the minibuffer can be more
  ;; than one line, causing the modeline to move up and down:

  ;; (add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target)
  ;; (setq eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)

  :config

  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :ensure
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package wgrep
  :ensure)

(use-package direnv
  :ensure
  :config
  (direnv-mode))

(use-package bazel
  :ensure (bazel-mode :type git :host github :repo "bazelbuild/emacs-bazel-mode"))

(use-package c-ts-mode
  :custom
  ;; 2 space indentation makes me want to die.
  (c-ts-mode-indent-offset 4)
  (c-ts-mode-indent-style 'linux)
  ;; Override the default c/c++ modes that emacs comes with.
  (add-to-list 'major-mode-remap-alist '(c-mode . c-ts-mode))
  (add-to-list 'major-mode-remap-alist '(c++-mode . c++-ts-mode))
  (add-to-list 'major-mode-remap-alist
               '(c-or-c++-mode . c-or-c++-ts-mode)))

(use-package tramp
  :config
  (setq remote-file-name-inhibit-locks t
        tramp-use-scp-direct-remote-copying t
        remote-file-name-inhibit-auto-save-visited t)
  (setq tramp-copy-size-limit (* 1024 1024) ;; 1MB
        tramp-verbose 2)
  (connection-local-set-profile-variables
   'remote-direct-async-process
   '((tramp-direct-async-process . t)))

  (connection-local-set-profiles
   '(:application tramp :protocol "scp")
   'remote-direct-async-process)

  (setq magit-tramp-pipe-stty-settings 'pty))


;;;###autoload
(add-to-list 'auto-mode-alist (cons "\\.go\\'" 'go-ts-mode))
(add-to-list 'auto-mode-alist (cons "\\.star\\'" 'bazel-mode))
(add-to-list 'auto-mode-alist (cons "\\.bazel\\'" 'bazel-mode))
(add-hook 'go-ts-mode (lambda () (electric-indent-local-mode -1)))
(setq go-ts-mode-indent-offset 4)

;; Need to tell emacs where to get the grammars
(setq treesit-language-source-alist
      '((go "https://github.com/tree-sitter/tree-sitter-go" "v0.20.0")
        (gomod "https://github.com/camdencheek/tree-sitter-go-mod" "v1.1.0")
        (starlark "https://github.com/tree-sitter-grammars/tree-sitter-starlark.git" "v1.1.0")
        (rust "https://github.com/tree-sitter/tree-sitter-rust" "v0.21.2")
        (cpp "https://github.com/tree-sitter/tree-sitter-cpp" "v0.22.0" "src")
        (c "https://github.com/tree-sitter/tree-sitter-c" "v0.23.2" "src")
        (bash "https://github.com/tree-sitter/tree-sitter-bash")
        (cmake "https://github.com/uyha/tree-sitter-cmake")
        (elisp "https://github.com/Wilfred/tree-sitter-elisp")
        (json "https://github.com/tree-sitter/tree-sitter-json")
        (make "https://github.com/alemuller/tree-sitter-make")
        (markdown "https://github.com/ikatyang/tree-sitter-markdown")
        (toml "https://github.com/tree-sitter/tree-sitter-toml")
        (yaml "https://github.com/ikatyang/tree-sitter-yaml")))
