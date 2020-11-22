;; Redirect the custom-set-variables to custom.el
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file t)

;;#########################
;;                        #
;;    Visual clean-up     #
;;                        #
;;#########################

(setq inhibit-startup-message t) ;  No startup message. Thanks but no thanks.

(scroll-bar-mode -1)  ; Disable scrollbar
(tool-bar-mode -1)    ; Disable toolbar
(tooltip-mode -1)     ; Disable tooltip
(set-fringe-mode 10)  ; Give some space
(menu-bar-mode -1)    ; Disable the menu bar

;; Frame transparency - BETTER CLEAN UP YOUR DESKTOP (I see you Max)
;; Comment this out in order to get rid of the effect or set the alpha's values to 100
(set-frame-parameter (selected-frame) 'fullscreen 'maximized)
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Custom function to change transparency
(defun change-transparency (n)
  "change transparency to a given value between 0 and 100"
  (interactive "nValue: ")
  (set-frame-parameter nil 'alpha `(,n . ,n))
  (add-to-list 'default-frame-alist `(alpha . (,n . ,n))))


;; Typo
(set-face-attribute 'default nil :font "Fira Code Retina" :height 100) ; /!\ Needs to be installed on the computer FIRST

;; enable line highlight, line number and column number
(global-hl-line-mode t)                ; Enable line number at the left
(global-display-line-numbers-mode t)   ; Enable display of line number (at the bottom)
(column-number-mode t)                 ; Enable column number  (at the bottom)

;; Disable line mode for some modes
(dolist (mode '(org-mode-hook
		term-mode-hook
		shell-mode-hook
		eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))


;;#########################
;;                        #
;;    PACKAGE MANAGING    #
;;                        #
;;#########################

;; Initialize package sources
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			 ("melpa-stable" . "https://stable.melpa.org/packages/")
			 ("org" . "https://orgmode.org/elpa/")
			 ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(use-package command-log-mode) ;; shows what I press (basically)

;;#########################
;;                        #
;;       IVY SETUP        #
;;                        #
;;#########################
;; Adds a lot of nice things like swiper
;; Ivy set-up
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

;; Ivy-rich
(use-package ivy-rich
  :init
  (ivy-rich-mode 1))

;; Ivy-enhanced version of common Emacs commands
;; It is simply a kind of better default
(use-package counsel
  :bind (("M-x" . counsel-M-x)
         ("C-x b" . counsel-ibuffer)
         ("C-x C-f" . counsel-find-file)
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history)))

;; Use helpful to get a better help mode in Emacs
;; Kind of better default help mode
(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 40)))

(use-package doom-themes
  :init (load-theme 'doom-dracula t))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; Shows all keybindings with 1 second delay
(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 1))

;; First time this is loaded on a new machine, needs to run :
;; M-x all-the-icons-install-fonts to download the fonts
;; THEN install them manually
(use-package all-the-icons)

;;#########################
;;                        #
;;     DISCORD SETUP      #
;;                        #
;;#########################
;; Discord rich presence
(use-package elcord)
(elcord-mode)

;; Discord integration for Emacs

;;#########################
;;                        #
;;    TREEMACS SETUP      #
;;                        #
;;#########################

(use-package treemacs
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (progn
    (setq treemacs-collapse-dirs                 (if treemacs-python-executable 3 0)
          treemacs-deferred-git-apply-delay      0.5
          treemacs-directory-name-transformer    #'identity
          treemacs-display-in-side-window        t
          treemacs-eldoc-display                 t
          treemacs-file-event-delay              5000
          treemacs-file-extension-regex          treemacs-last-period-regex-value
          treemacs-file-follow-delay             0.2
          treemacs-file-name-transformer         #'identity
          treemacs-follow-after-init             t
          treemacs-git-command-pipe              ""
          treemacs-goto-tag-strategy             'refetch-index
          treemacs-indentation                   2
          treemacs-indentation-string            " "
          treemacs-is-never-other-window         nil
          treemacs-max-git-entries               5000
          treemacs-missing-project-action        'ask
          treemacs-move-forward-on-expand        nil
          treemacs-no-png-images                 nil
          treemacs-no-delete-other-windows       t
          treemacs-project-follow-cleanup        nil
          treemacs-persist-file                  (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
          treemacs-position                      'left
          treemacs-recenter-distance             0.1
          treemacs-recenter-after-file-follow    nil
          treemacs-recenter-after-tag-follow     nil
          treemacs-recenter-after-project-jump   'always
          treemacs-recenter-after-project-expand 'on-distance
          treemacs-show-cursor                   nil
          treemacs-show-hidden-files             t
          treemacs-silent-filewatch              nil
          treemacs-silent-refresh                nil
          treemacs-sorting                       'alphabetic-asc
          treemacs-space-between-root-nodes      t
          treemacs-tag-follow-cleanup            t
          treemacs-tag-follow-delay              1.5
          treemacs-user-mode-line-format         nil
          treemacs-user-header-line-format       nil
          treemacs-width                         35
          treemacs-workspace-switch-cleanup      nil)

    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    ;;(treemacs-resize-icons 44)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode t)
    (pcase (cons (not (null (executable-find "git")))
                 (not (null treemacs-python-executable)))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple))))
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))

(use-package treemacs-icons-dired
  :after treemacs dired
  :ensure t
  :config (treemacs-icons-dired-mode))(use-package treemacs
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (progn
    (setq treemacs-collapse-dirs                 (if treemacs-python-executable 3 0)
          treemacs-deferred-git-apply-delay      0.5
          treemacs-directory-name-transformer    #'identity
          treemacs-display-in-side-window        t
          treemacs-eldoc-display                 t
          treemacs-file-event-delay              5000
          treemacs-file-extension-regex          treemacs-last-period-regex-value
          treemacs-file-follow-delay             0.2
          treemacs-file-name-transformer         #'identity
          treemacs-follow-after-init             t
          treemacs-git-command-pipe              ""
          treemacs-goto-tag-strategy             'refetch-index
          treemacs-indentation                   2
          treemacs-indentation-string            " "
          treemacs-is-never-other-window         nil
          treemacs-max-git-entries               5000
          treemacs-missing-project-action        'ask
          treemacs-move-forward-on-expand        nil
          treemacs-no-png-images                 nil
          treemacs-no-delete-other-windows       t
          treemacs-project-follow-cleanup        nil
          treemacs-persist-file                  (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
          treemacs-position                      'left
          treemacs-recenter-distance             0.1
          treemacs-recenter-after-file-follow    nil
          treemacs-recenter-after-tag-follow     nil
          treemacs-recenter-after-project-jump   'always
          treemacs-recenter-after-project-expand 'on-distance
          treemacs-show-cursor                   nil
          treemacs-show-hidden-files             t
          treemacs-silent-filewatch              nil
          treemacs-silent-refresh                nil
          treemacs-sorting                       'alphabetic-asc
          treemacs-space-between-root-nodes      t
          treemacs-tag-follow-cleanup            t
          treemacs-tag-follow-delay              1.5
          treemacs-user-mode-line-format         nil
          treemacs-user-header-line-format       nil
          treemacs-width                         35
          treemacs-workspace-switch-cleanup      nil)

    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    ;;(treemacs-resize-icons 44)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode t)
    (pcase (cons (not (null (executable-find "git")))
                 (not (null treemacs-python-executable)))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple))))
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))

(use-package treemacs-icons-dired
  :after treemacs dired
  :ensure t
  :config (treemacs-icons-dired-mode))

(treemacs)

;;#########################
;;                        #
;;     GOD-MODE SETUP     #
;;                        #
;;#########################
;; Use god-mode in order to reduce the usage of ctrl key.
;; Now Emacs assumes a C key before each key pressed :
;; f = C-f
;; x s = C-x C-s
;; x SPC s = C-x s
;; g = M-
;; G = C-M-
;;(use-package god-mode
;;  :config (setq god-mode-enable-function-key-translation nil)) ; Disable god-mode for function key
;;(god-mode)
;; ESC key to toggle god-mode
;;(global-set-key (kbd "<escape>") #'god-local-mode)

;; Ensure that no buffers are skipped
;;(setq god-exempt-major-modes nil)
;;(setq god-exempt-predicates nil)

;; Enable a cursor style that shows if god-mode is on
;;(defun my-god-mode-update-cursor ()
;;  (setq cursor-type (if (or god-local-mode buffer-read-only)
;;                        'box
;;                      'bar)))

;;(add-hook 'god-mode-enabled-hook #'my-god-mode-update-cursor)
;;(add-hook 'god-mode-disabled-hook #'my-god-mode-update-cursor)




;;#########################
;;                        #
;;    KEYBINDING SETUP    #
;;                        #
;;#########################

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; Using general-define-key for a better (and cleaner) rebind
(use-package general)

(general-define-key
 "C-x p" 'counsel-switch-buffer    ; Make C-x p use the counsel-switch-buffer
 "C-s" 'counsel-grep-or-swiper)


;;#########################
;;                        #
;;    PROJECTILE SETUP    #
;;                        #
;;#########################
;; Projectile is useful in order to manage project
;; Mainly installed because lsp-mode uses it for project management

(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy))
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  ;; NOTE: Set this to the folder where you keep your Git repos!
  (when (file-directory-p "D:\Projects")
    (setq projectile-project-search-path '("D:\Projects")))
  (setq projectile-switch-project-action #'projectile-dired))

(use-package counsel-projectile
  :config (counsel-projectile-mode))


;;#########################
;;                        #
;;       MAGIT SETUP      #
;;                        #
;;#########################
;; It's a kind of magit !
;; Nice git integration to Emacs
;; hit C-x g to access Magit interface

(use-package magit
  :commands (magit-status magit-get-current-branch)
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))


;;#########################
;;                        #
;;    IDE/CODE SETUP      #
;;                        #
;;#########################
;; lsp-mode
;; set prefix for lsp-command-keymap
(setq lsp-keymap-prefix "C-c l")

(use-package lsp-mode
    :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
            ;;(XXX-mode . lsp)
            ;; if you want which-key integration
            (lsp-mode . lsp-enable-which-key-integration))
    :commands lsp)

;; Automatically add ending brackets and braces
(electric-pair-mode 1)

(use-package flycheck)

(use-package lsp-ui :commands lsp-ui-mode)

(use-package lsp-ivy :commands lsp-ivy-workspace-symbol)

(use-package lsp-treemacs :commands lsp-treemacs-errors-list)

(use-package yasnippet :config (yas-global-mode))

(use-package company)
(add-hook 'after-init-hook 'global-company-mode)


(use-package dap-mode)
;; (use-package dap-LANGUAGE) to load the dap adapter for your language

;; Java setup
(use-package lsp-java)
(add-hook 'java-mode-hook #'lsp)

;; Python setup
(use-package jedi)
(require 'jedi)
(setq jedi:server-args
      '("--sys-path" "/usr/lib/python2.7/"
        "--sys-path" "/usr/lib/python2.7/site-packages"))

(jedi:ac-setup)
(jedi:complete)



;;#########################
;;                        #
;;    ORG-MODE SETUP      #
;;                        #
;;#########################

(use-package org
  :hook (org-mode . efs/org-mode-setup)
  :config
  (setq org-ellipsis " ▾")
  (efs/org-font-setup))

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))
