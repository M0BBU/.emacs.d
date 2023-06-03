; Sane defaults?
(column-number-mode 1)
(display-time-mode 1)
(show-paren-mode 1)
(display-battery-mode 1)              

; Get rid of top ui and welcome message
(tooltip-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)

;; Scroll by line?
(setq scroll-step 1)

;; macOS modifier keys - ergonomics!
(setq mac-command-modifier 'meta) ; Command == Meta
(setq mac-option-modifier 'super) ; make opt key do Super

(defalias 'yes-or-no-p 'y-or-n-p)

;; Maximize on startup
(add-hook 'window-setup-hook 'toggle-frame-maximized t)

;; Font stuff!
(set-face-attribute 'default nil :font "Menlo 16")

(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

(setq ring-bell-function 'ignore)

(delete-selection-mode +1)

;; Package manager and org mode?
(setq package-enable-at-startup nil)

;; 80 column rule
(setq-default fill-column 80)
(global-display-fill-column-indicator-mode t)

(global-display-line-numbers-mode t)

;; Don't polute directory with backup files
(setq
   backup-by-copying t      ; don't clobber symlinks
   backup-directory-alist
    '(("." . "~/.emacs.d/backups"))    ; don't litter my fs tree
   delete-old-versions t
   kept-new-versions 6
   kept-old-versions 2
   version-control t)       ; use versioned backups


;; Cool remappings
;; Sets M-o to switch windows
(global-set-key "\M-o" 'other-window)
;; Save and close buffer immediately
(global-set-key "\C-x\C-k" (lambda () (interactive)
                             (save-buffer)
                             (kill-this-buffer)))
;; Scrolls one line without moving cursor
(global-set-key (kbd "M-n") #'scroll-up-line)
(global-set-key (kbd "M-p") #'scroll-down-line)

;; cc-mode defaults
(setq c-default-style "stroustrup")
(add-hook 'c++-mode-hook (lambda nil (setq c-basic-offset 4)))

(when (>= emacs-major-version 24)
  (require 'package)
  (package-initialize)
  (add-to-list 'package-archives
               '("melpa" . "http://melpa.org/packages/") t)
  (add-to-list 'package-archives
               '("gnu" . "http://elpa.gnu.org/packages/") t)
  (add-to-list 'package-archives
               '("elpa" . "http://elpa.gnu.org/packages/") t)
  (add-to-list 'package-archives
               '("org" . "http://orgmode.org/elpa/") t)
  )

;; use-package to manages package configurations?
(unless (package-installed-p `use-package)
  (package-refresh-contents)
  (package-install 'use-package)
  (package-install 'zenburn-theme)
  (package-install 'pdf-tools)
  (package-install 'auctex)
  (package-install 'rust-mode)
  (package-install 'haskell-mode)
  (package-install 'magit)
  (package-install 'vertico)
  (package-install 'orderless)
  (package-install 'marginalia)
  (package-install 'elfeed)
  (package-install 'markdown-mode)
  (package-install 'cuda-mode)
  (package-install 'projectile)
  (package-install 'flx-ido)
  (package-install 'go-mode)
  )

;; Color scheme
(use-package zenburn-theme
  :init
  (load-theme 'zenburn t))

(use-package vertico
  :init
  (vertico-mode +1))

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :init
  (savehist-mode))

;; Optionally use the `orderless' completion style.
(use-package orderless
  :init
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq completion-styles '(orderless flex)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

;; Enable rich annotations using the Marginalia package
(use-package marginalia
  ;; Either bind `marginalia-cycle' globally or only in the minibuffer
  :bind (("M-A" . marginalia-cycle)
         :map minibuffer-local-map
         ("M-A" . marginalia-cycle))

  ;; The :init configuration is always executed (Not lazy!)
  :init

  ;; Must be in the :init section of use-package such that the mode gets
  ;; enabled right away. Note that this forces loading the package.
  (marginalia-mode))

;; Better matching
(require 'flx-ido)
(ido-mode 1)
(ido-everywhere 1)
(flx-ido-mode 1)
;; disable ido faces to see flx highlights.
(setq ido-enable-flex-matching t)
(setq ido-use-faces nil)

(use-package tramp
  :config
  (setq tramp-default-method "ssh")
  (setq tramp-ssh-controlmaster-options
        (concat
         "-o ControlPath=~/.ssh/master-%%r@%%h:%%p "
         "-o ControlMaster=auto -o ControlPersist=yes"))
  :defer 1  ; lazy loading
  )

(projectile-mode +1)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

(defun CAEN-connect()
  "Connect to CAEN as long as you have an open ssh connection to CAEN"
  (interactive)
  (find-file "/ssh:dchoo@login.engin.umich.edu:"))

(load "~/.emacs.d/rss.el")
(load_rss_feeds)

;; No blurry pdf in emacs!
(setq-default pdf-view-use-scaling t)
 ;; to use pdfview with auctex
(setq TeX-view-program-selection '((output-pdf "PDF Tools"))
      TeX-view-program-list '(("PDF Tools" TeX-pdf-tools-sync-view))
      TeX-source-correlate-start-server t)
;; not sure if last line is neccessary
;; to have the buffer refresh after compilation
(add-hook 'TeX-after-compilation-finished-functions
          #'TeX-revert-document-buffer)
;; LaTeX mode
(add-hook 'LaTeX-mode-hook 'auto-fill-mode) ; Automatically break lines

(setq TeX-electric-math (cons "$" "$"))
(setq TeX-electric-sub-and-superscript t)

(require 'ansi-color)
(defun endless/colorize-compilation ()
  "Colorize from `compilation-filter-start' to `point'."
  (let ((inhibit-read-only t))
    (ansi-color-apply-on-region
     compilation-filter-start (point))))

(add-hook 'compilation-filter-hook
          #'endless/colorize-compilation)


;; Load in CMake mode
;; Change to proper path
(setq load-path (cons (expand-file-name "/opt/homebrew/Cellar/cmake/3.26.3/share/emacs/site-lisp/cmake") load-path))
(require 'cmake-mode)

;; EOF

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(cuda-mode flx-ido go-mode marginalia realgud-lldb realgud pdf-tools orderless vertico haskell-mode magit rust-mode ## auctex zenburn-theme use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :extend nil :stipple nil :background "#1C1C1C" :foreground "#DCDCCC" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 180 :width normal :foundry "nil" :family "Menlo"))))
 '(fringe ((t (:background "#000000" :foreground "#DCDCCC"))))
 '(line-number ((t (:inherit default :background "#1C1C1C" :foreground "#6F6F6F")))))
