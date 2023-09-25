;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")
        ("melpa" . "https://melpa.org/packages/")))

(eval-when-compile
  (add-to-list 'load-path "~/.config/emacs/use-package")
  (require 'use-package))

(setq use-package-always-ensure t)

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
    "fd" 'dired  ; open directory browser
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
    ;; Vterm
    "vt" 'vterm
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

;; vterm
(use-package vterm)

;; Enable eglot for language server support
(use-package eglot
  :commands eglot)

;; Julia
(use-package julia-mode
  :mode "\\.jl\\'")
(use-package eglot-jl
  :after eglot julia-mode)
(use-package julia-snail
  :after vterm julia-mode
  :hook (julia-mode . julia-snail-mode))


;; Enable company for auto-completion
(use-package company
  :config
  (add-hook 'after-init-hook 'global-company-mode))

;; Enable flycheck for syntax checking
(use-package flycheck
  :init
  (global-flycheck-mode))

;; Enable magit for Git integration
(use-package magit
  :bind (("C-x g" . magit-status)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 05 UI
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'load-path (concat user-emacs-directory "nano-emacs"))
(require 'nano)
(menu-bar-mode 0)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 06 Custom Variables & Faces
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file 'noerror)


