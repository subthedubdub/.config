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
1
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

;; projectile
(use-package projectile)

;; lsp / dap
(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-c l")
  :hook ((lsp-mode . lsp-enable-which-key-integration))
  :commands lsp)
(use-package lsp-treemacs :commands lsp-treemacs-errors-list)
(use-package lsp-ui
  :ensure
  :commands lsp-ui-mode
  :custom
  (lsp-ui-peek-always-show t)
  (lsp-ui-sideline-show-hover t)
  (lsp-ui-doc-enable nil))
(use-package exec-path-from-shell
  :init (exec-path-from-shell-initialize))

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
(use-package dap-julia
  :straight (:host github :repo "emacs-lsp/dap-mode"))


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
(use-package helm)

;; NANO
(straight-use-package
 '(nano-theme :type git :host github
               :repo "rougier/nano-theme"))
(straight-use-package
 '(nano-agenda :type git :host github
	       :repo "rougier/nano-agenda"))
(straight-use-package
 '(nano-read :type git :host github
	       :repo "rougier/nano-read"))
(straight-use-package
 '(nano-modeline :type git :host github
		 :repo "rougier/nano-modeline"
		 :branch "rewrite"))
(straight-use-package
 '(nano-box :type git :host github
		 :repo "rougier/nano-tools"))
(load-theme 'nano t)
(require 'nano-modeline)
(setq nano-modeline-position #'nano-modeline-footer)
(add-hook 'prog-mode-hook            #'nano-modeline-prog-mode)
(add-hook 'text-mode-hook            #'nano-modeline-text-mode)
(add-hook 'org-mode-hook             #'nano-modeline-org-mode)
(add-hook 'pdf-view-mode-hook        #'nano-modeline-pdf-mode)
(add-hook 'mu4e-headers-mode-hook    #'nano-modeline-mu4e-headers-mode)
(add-hook 'mu4e-view-mode-hook       #'nano-modeline-mu4e-message-mode)
(add-hook 'elfeed-show-mode-hook     #'nano-modeline-elfeed-entry-mode)
(add-hook 'elfeed-search-mode-hook   #'nano-modeline-elfeed-search-mode)
(add-hook 'term-mode-hook            #'nano-modeline-term-mode)
(add-hook 'xwidget-webkit-mode-hook  #'nano-modeline-xwidget-mode)
(add-hook 'messages-buffer-mode-hook #'nano-modeline-message-mode)
(add-hook 'org-capture-mode-hook     #'nano-modeline-org-capture-mode)
(add-hook 'org-agenda-mode-hook      #'nano-modeline-org-agenda-mode)


(use-package org-modern
  :config
  (add-hook 'org-mode-hook #'org-modern-mode)
  (add-hook 'org-agenda-finalize-hook #'org-modern-agenda))



(setq-default select-enable-clipboard t) ; Merge system's and Emacs' clipboard

(set-frame-font "JuliaMono 16" nil t)

(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)
(column-number-mode t)
(setq inhibit-startup-screen t)
(blink-cursor-mode 0)
(setq-default cursor-in-non-selected-windows nil ; Hide the cursor in inactive windows
              cursor-type '(hbar . 2)            ; Underline-shaped cursor
              cursor-intangible-mode t           ; Enforce cursor intangibility
              x-stretch-cursor nil)              ; Don't stretch cursor to the glyph width


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 06 Custom Variables & Faces
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file 'noerror)


