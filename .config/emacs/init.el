3;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 00 General
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq user-full-name "Michael Tomas Kovarik"
      user-mail-address "michaelkovarik@protonmail.com")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 01 Package Management
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'package)

(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
        ("gnu-devel" . "https://elpa.gnu.org/devel/")
        ("Nongnu" . "https://elpa.nongnu.org/nongnu/")
        ("melpa" . "https://melpa.org/packages/")))
(eval-when-compile
  (add-to-list 'load-path "~/.config/emacs/use-package")
  (require 'use-package))

(setq use-package-always-ensure t)

;; straight
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))


;; Use straight.el with use-package
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 02 Keyboard and Mouse
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package general
  :after evil
  :config
  (general-evil-setup t)
  (general-create-definer my-leader-def
    :states '(normal insert visual emacs)
    :keymaps 'override
    :prefix "SPC"
    :global-prefix "M-SPC")
  ;; Keyboard Bindings
  (my-leader-def
    ;; File operations
    "ff" 'find-file  ; find file
    "fs" 'save-buffer  ; save file
    "fw" 'write-file  ; write file
    "fr" 'recentf-open-files  ; open recent file
    "fd" 'dired  ; open directory browser.
    "ft"  'treemacs ; toggle treemacs

    ;; Buffer operations
    "bb" 'switch-to-buffer  ; switch to another buffer
    "bd" 'kill-buffer  ; kill a buffer
    "bs" 'save-buffer  ; save a buffer

    ;; Window operations
    "ws" 'split-window-below  ; split window horizontally
    "wv" 'split-window-right  ; split window vertically
    "wd" 'delete-window  ; delete window
    "wq" 'kill-buffer-and-window  ; kill the current buffer and window
    "wo" 'delete-other-windows  ; delete other windows
    "wl" 'evil-window-right
    "wh" 'evil-window-left
    "wj" 'evil-window-down
    "wk" 'evil-window-up
    ;; Terminal
    "tm" 'eat
    ))



(use-package evil
  :init
  (setq evil-want-keybinding nil) ; This is needed for evil-collection
  :config
    (evil-mode 1)
    (define-key evil-normal-state-map (kbd "C-g") 'evil-normal-state))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package which-key
  :config
  (which-key-mode))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 03 Autosaves and Backups
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Directory to put auto-save files and backups
(setq save-directory (concat user-emacs-directory ".saves/"))
(if (not (file-exists-p save-directory))
    (make-directory save-directory t))

;; Auto-save files
(setq auto-save-file-name-transforms
      `((".*" ,(concat save-directory "\\1") t)))

;; Backup files
(setq backup-directory-alist
      `((".*" . ,save-directory)))
(setq backup-by-copying t)  ; Don't delink hardlinks
(setq version-control t)  ; Use version numbers on backups
(setq delete-old-versions t)  ; Automatically delete excess backups
(setq kept-new-versions 20)  ; How many of the newest versions to keep
(setq kept-old-versions 5)  ; And how many of the old


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 04 Coding
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; eat
(use-package eat)

;; eglot
(use-package eglot
  :hook ((python-mode . eglot-ensure)
	 (java-mode . eglot-ensure)
	 (julia-mode . eglot-ensure)
         (after-save . eglot-format))
  :config
  (setq eglot-report-progress nil)
  (add-to-list 'eglot-server-programs
               '(python-mode . ("ruff" "server"))))

;; dap
(use-package dap-mode
  :config
  (dap-ui-mode)
  (dap-ui-controls-mode 1))


;; Julia
;; dap-julia not released until dap-mode 0.9
(use-package julia-mode
  :mode "\\.jl\\'")
(use-package lsp-julia :config (add-hook 'julia-mode-hook 'lsp))
(use-package julia-snail
  :after vterm julia-mode
  :hook (julia-mode . julia-snail-mode))

(use-package eglot
  :hook ((python-mode . eglot-ensure)
         (after-save . eglot-format))
  :config
  (add-to-list 'eglot-server-programs
               '(python-mode . ("ruff" "server"))))

;; Enable company for auto-completion
(use-package company
  :config
  (add-hook 'after-init-hook 'global-company-mode))

;; Enable flycheck for syntax checking
(use-package flycheck
  :init
  (global-flycheck-mode))

;; Enable magit for Git integration
(use-package magit)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 05 UI
;; MUST have JuliaMono font installed
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package helm)

;; beep boop
(setq visible-bell 1)

;; Theming
(use-package gruvbox-theme
  :config
  (load-theme 'gruvbox-light-soft t))
(straight-use-package
 '(nano-agenda :type git :host github
	       :repo "rougier/nano-agenda"))
(modify-all-frames-parameters
 '((right-dvidider-width . 30)
   (left-fringe . 30)
   (right-fringe . 30)))

(use-package nerd-icons)
(use-package org-modern
  :config
  (add-hook 'org-mode-hook #'org-modern-mode)
  (add-hook 'org-agenda-finalize-hook #'org-modern-agenda))
(use-package doom-modeline
  :config
  (add-hook 'after-init-hook #'doom-modeline-mode))

;; Eldoc enhancements
(use-package eldoc-box
  :hook (eldoc-mode . eldoc-box-hover-at-point-mode))


(setq-default select-enable-clipboard t) ; Merge system's and Emacs' clipboard

(set-frame-font "JuliaMono 16" nil t)

(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)
(column-number-mode t)
(setq inhibit-startup-screen t)
(blink-cursor-mode 0)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 06 Custom Variables & Faces
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file 'noerror)
