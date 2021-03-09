;; Redirect the custom-set-variables to custom.els
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file t)
;; Create a variable to indicate where emacs's configuration is installed
(setq EMACS_DIR "~/.emacs.d/")


;;#########################
;;                        #
;;   PACKAGE MANAGEMENT   #
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


;;#########################
;;                        #
;;       QOL SETUP        #
;;                        #
;;#########################


;;---===>>><<<===---
;;    Default Emacs changes
;;---===>>><<<===---

(setq inhibit-startup-message t)                              ;No startup message. Thanks but no thanks.
(set-frame-parameter (selected-frame) 'fullscreen 'maximized) ; Start Emacs in fullscreen
(add-to-list 'default-frame-alist '(fullscreen . maximized))  

(scroll-bar-mode -1)                    ; Disable scrollbar
(tool-bar-mode -1)                      ; Disable toolbar
(tooltip-mode -1)                       ; Disable tooltip
(set-fringe-mode 10)                    ; Give some space
(menu-bar-mode -1)                      ; Disable the menu bar
(display-time)                          ; Display time in active modline (AM/PM)
(setq-default cursor-type 'bar)         ; sweet cursor
(defalias 'yes-or-no-p 'y-or-n-p)       ; Accept 'y' in lieu of 'yes'
(setq-default indent-tabs-mode nil)     ; No tabs, only spaces !
(setq tab-width 4)                      ; I like a 4 indentation level

;; UTF-8 should always be the default
(set-charset-priority 'unicode)
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(setq default-process-coding-system '(utf-8-unix . utf-8-unix))

;; Move all the backup files to specific cache directory
;; This way you won't have annoying temporary files starting with ~(tilde) in each directory
;; Following setting will move temporary files to specific folders inside cache directory in EMACS_DIR

(setq user-cache-directory (concat EMACS_DIR "cache"))
(setq backup-directory-alist `(("." . ,(expand-file-name "backups" user-cache-directory)))
      url-history-file (expand-file-name "url/history" user-cache-directory)
      auto-save-list-file-prefix (expand-file-name "auto-save-list/.saves-" user-cache-directory)
      projectile-known-projects-file (expand-file-name "projectile-bookmarks.eld" user-cache-directory))



;; Typo - go to https://www.jetbrains.com/lp/mono/#how-to-install to get it !
(set-face-attribute 'default nil :font "JetBrains Mono-12.0" :height 100)

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



;; Custom function to change transparency
(defun change-transparency (n)
  "change transparency to a given value between 0 and 100"
  (interactive "nValue: ")
  (set-frame-parameter nil 'alpha `(,n . ,n))
  (add-to-list 'default-frame-alist `(alpha . (,n . ,n))))


;; credit goes to : http://ergoemacs.org/emacs/emacs_kill-ring.html
(defun my-delete-word (arg)
  "Delete characters forward until encountering the end of a word.
With argument, do this that many times.
This command does not push erased text to kill-ring."
  (interactive "p")
  (delete-region (point) (progn (forward-word arg) (point))))

;; credit goes to : http://ergoemacs.org/emacs/emacs_kill-ring.html
(defun my-backward-delete-word (arg)
  "Delete characters backward until encountering the beginning of a word.
With argument, do this that many times.
This command does not push erased text to kill-ring."
  (interactive "p")
  (my-delete-word (- arg)))

;; credit goes to : http://ergoemacs.org/emacs/emacs_kill-ring.html
(defun my-delete-line ()
  "Delete text from current position to end of line char."
  (interactive)
  (delete-region
   (point)
   (save-excursion (move-end-of-line 1) (point)))
  (delete-char 1)
)

;; credit goes to : http://ergoemacs.org/emacs/emacs_kill-ring.html
(defun my-delete-line-backward ()
  "Delete text between the beginning of the line to the cursor position."
  (interactive)
  (let (x1 x2)
    (setq x1 (point))
    (move-beginning-of-line 1)
    (setq x2 (point))
    (delete-region x1 x2)))

;; Custom function to kill current buffer
(defun kill-this-buffer ()
  "Kill the current buffer."
  (interactive)
  (kill-buffer nil)
  )

;;---===>>><<<===---
;;    QOL Packages
;;---===>>><<<===---


;; This will help eliminate weird escape sequences during compilation of projects.
;; Source : https://github.com/neppramod/java_emacs/blob/master/emacs-configuration.org
(defun vs/ansi-colorize-buffer ()
(let ((buffer-read-only nil))
(ansi-color-apply-on-region (point-min) (point-max))))

(use-package ansi-color
:ensure t
:config
(add-hook 'compilation-filter-hook 'vs/ansi-colorize-buffer)
)


;;highlight active window !
(use-package dimmer)
(dimmer-mode t)

;; This package allows to go ignore comment when going to end-of-line
(use-package mwim)

;; A simple Emacs cheatsheet, may be useful to remember some kbd
(use-package cheatsheet)
(cheatsheet-add :group 'Org-mode
                :key "Shit-Tab"
                :description "Cycle show/hide all header")
(cheatsheet-add :group 'Org-mode
                :key "C-return"
                :description "Creates a new heading at the same level under current heading")
(cheatsheet-add :group 'Org-mode
                :key "C-M-<arrow-key>"
                :description "Move header") ; org-metaup function
(cheatsheet-add :group 'Org-mode
                :key "C-c C-l"
                :description "Insert link on selected text, if no text selected ask for text")
(cheatsheet-add :group 'Org-mode
                :key "C-c C-o"
                :description "org-open-at-point opens link, footnote and so")
(cheatsheet-add :group 'Org-mode-tables
                :key "<tab>"
                :description "Reformat table, cycle through cell")
(cheatsheet-add :group 'Org-mode
                :key "S-<tab>"
                :description "The exact opposite of <tab>")
(cheatsheet-add :group 'Org-mode-list
                :key "M-<enter>"
                :description "Insert new item")
(cheatsheet-add :group 'Org-mode-list
                :key "C-c C-x C-b"
                :description "Toggle checkbox")
(cheatsheet-add :group 'Org-mode
                :key "C-c C-t"
                :description "Change header state (TODO/DONE/etc...)")
(cheatsheet-add :group 'Org-mode
                :key "S-<arrow-key>"
                :description "Change header state too")
                
;; winner mode !!
(winner-mode) ;;undo = C-c-left , redo = C-c-right

;; Adds a lot of nice things like swiper
;; Ivy set-up
(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
	 :map ivy-minibuffer-map
	 ("TAB" . ivy-alt-done)
	 ("C-l" . ivy-alt-done) 
	 ("C-n" . ivy-next-line)
	 ("C-p" . ivy-previous-line)
	 :map ivy-switch-buffer-map
         ("C-p" . ivy-previous-line)
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-p" . ivy-previous-line)
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

;; Shows all keybindings with 1 second delay
(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 1))


;;---===>>><<<===---
;;    Keybindings
;;---===>>><<<===---

;; Using key-cord for non-prefixe command binding
(use-package use-package-chords
:ensure t
:init 
:config (key-chord-mode 1)
(setq key-chord-two-keys-delay 0.4)
(setq key-chord-one-key-delay 0.5) ; default 0.2
)

;; Using general-define-key for a better (and cleaner) rebind
(use-package general)


(general-unbind
  "C-z"
  "C-k"
  "C-K"
  "M-d"
  "M-<backspace>"
  "C-<backspace>"
  ;; "M-f"           ; I want to replace (forward-word) by (forward-symbol)
  ;; "M-b"           ; I want to replace (backward-word) by (backward-symbol)
  )

;; Avy for better navigation!
(use-package avy 
:ensure t
:chords
("jc" . avy-goto-char)
("jz" . avy-goto-word-1)
("jl" . avy-goto-line))

;; C- key
(general-define-key
 "C-e" 'mwim-end
 "C-a" 'mwim-beginning
 "C-s" 'counsel-grep-or-swiper
 "C-c /" 'comment-dwim
 "C-x k" 'kill-this-buffer
 "C-x K" 'kill-buffer
 "C-k" 'my-delete-line
 "C-K" 'my-delete-line-backward
 )

;; M- key
(general-define-key
 ;; Use M-arrow to move between windows
 "<M-right>" 'windmove-right
 "<M-left>" 'windmove-left
 "<M-up>" 'windmove-up
 "<M-down>" 'windmove-down
 "M-d" 'my-delete-word
 "M-<backspace>" 'my-backward-delete-word
 "C-<backspace>" 'my-backward-delete-word
 )

;; NOTE : there are only basic/generic keybinding listed here.
;;        Other more specific keybinding may be found under other sections.

;;#########################
;;                        #
;;   VISUAL IMPROVEMENT   #
;;                        #
;;#########################

;; A better modeline !
(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 40)))

;;---===>>><<<===---
;;    Themes
;;---===>>><<<===---
;; use M-x counsel-load-theme RET to interactively switch theme
(use-package doom-themes)
(use-package monokai-theme)
(use-package kaolin-themes)
;; (load-theme 'doom-molokai t)
;; (load-theme 'doom-one t)
;; (load-theme 'spacemacs-dark t)
(load-theme 'atom-one-dark t)
(setq doom-molokai-brighter-modeline t)
(setq doom-molokai-brighter-comments t)
(setq doom-molokai-comment-bg t)

;; First time this is loaded on a new machine, needs to run :
;; M-x all-the-icons-install-fonts to download the fonts
;; THEN install them manually
(use-package all-the-icons)



;;#########################
;;                        #
;;    TREEMACS SETUP      #
;;                        #
;;#########################

;; Treemacs ! For a nice looking tree-view
(use-package treemacs)
(use-package treemacs-icons-dired)

(setq treemacs-display-in-side-window  t
      treemacs-width                   35
      treemacs-position                'left)

(setq treemacs-python-executable "C:\\Users\\lefan\\AppData\\Local\\Programs\\Python\\Python37-32\\python.exe")

(treemacs-follow-mode t)
(treemacs-icons-dired-mode t)

(treemacs)

(general-define-key
 :keymaps 'global-map
 "M-0" 'treemacs-select-window
 "C-x t t" 'treemacs)


;;#########################
;;                        #
;;       IDE SETUP        #
;;                        #
;;#########################

;;---===>>><<<===---
;;    Project management
;;---===>>><<<===---
;; Projectile is useful in order to manage project
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

;;---===>>><<<===---
;;    GIT integration
;;---===>>><<<===---
;; It's a kind of magit !
;; Nice git integration to Emacs
;; hit C-x g to access Magit interface
(use-package magit
  :commands (magit-status magit-get-current-branch)
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

;;---===>>><<<===---
;;    IDE Utilities
;;---===>>><<<===---

;; Package to help expand/collapse region
(use-package expand-region)
(general-define-key
 "C-+" 'expand-region)

;; Allow for a larger memory usage to read subprocess
(setq read-process-output-max (* 1024 1024)) ; 1mb

;; Color for parenthesis indentation
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; Show matching brackets and braces
(show-paren-mode 1)

;; Automatically add ending brackets and braces
(electric-pair-mode 1)

;;Quickrun to run & compile everything !
(use-package quickrun)

;; Allow to manage TODO, FIXME and so keywords
(use-package fixmee)
(use-package button-lock)
(global-fixmee-mode 1)


;; allow to fold some part of code
(use-package dash)
(use-package s)
(use-package origami)
(origami-mode 1)

;;---===>>><<<===---
;;    lsp-mode
;;---===>>><<<===---

;; lsp-mode
(use-package lsp-mode
  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
         ;;(XXX-mode . lsp)
	 (java-mode . #'lsp-deferred)
	 (python-mode . #'lsp-deferred)
         ;; if you want which-key integration
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp
  :init (setq
         lsp-keymap-prefix "C-c l"
         lsp-enable-file-watchers nil
         read-process-output-max(* 1024 1024)
         lsp-completion-provider :capf
         lsp-idle-delay 0.500
         ;; lsp-log-io nil
         lsp-print-performance t)
  :config 
    (setq lsp-intelephense-multi-root nil) ; don't scan unnecessary projects
    (with-eval-after-load 'lsp-intelephense
    (setf (lsp--client-multi-root (gethash 'iph lsp-clients)) nil))
	(define-key lsp-mode-map (kbd "C-c l") lsp-command-map))

(use-package lsp-ui)
(lsp-ui-peek-enable t)
(lsp-ui-doc-enable t)

(setq lsp-completion-provider :capf)
(setq lsp-completion-show-detail t)
(setq lsp-completion-show-kind t)

(use-package lsp-ivy :commands lsp-ivy-workspace-symbol)

(use-package lsp-treemacs :commands lsp-treemacs-errors-list)

;; The default is 800 kilobytes.  Measured in bytes.

(setq gc-cons-threshold 100000000) ;; 100 MB
(setq read-process-output-max (* 1 1024 1024)) ;; 1 MB

;;---===>>><<<===---
;;    Syntax checking
;;---===>>><<<===---

(use-package flycheck)

;;---===>>><<<===---
;;    Yasnippet
;;---===>>><<<===---

(use-package yasnippet)
(use-package yasnippet-snippets)

(use-package java-snippets)

(yas-global-mode 1)
(setq yas-snippet-dirs '("~/.emacs.d/snippets"))

;;---===>>><<<===---
;;    Code completion
;;---===>>><<<===---

(use-package company)
(global-company-mode t)
(add-hook 'after-init-hook 'global-company-mode)

;;---===>>><<<===---
;;     Debugger
;;---===>>><<<===---

(use-package dap-mode
  :ensure t
  :after (lsp-mode)
  :functions dap-hydra/nil
  :config
  (require 'dap-java)
  :bind (:map lsp-mode-map
         ("<f5>" . dap-debug)
         ("M-<f5>" . dap-hydra))
  :hook ((dap-mode . dap-ui-mode)
    (dap-session-created . (lambda (&_rest) (dap-hydra)))
    (dap-terminated . (lambda (&_rest) (dap-hydra/nil)))))


;;---===>>><<<===---
;;       Java
;;---===>>><<<===---

;; language server : jdtls
(use-package lsp-java)
(use-package dap-java :ensure nil)
(add-hook 'java-mode-hook #'lsp)

;; Gradle support
(use-package gradle-mode)
(defun build-and-run()
  "Build and run a gradle project"
  (interactive)
  (gradle-run "build run"))

;;---===>>><<<===---
;;      Python
;;---===>>><<<===---

(use-package elpy
  :bind (
         ("C-M-<up>" . elpy-nav-move-line-or-region-up)
         ("C-M-<down>" . elpy-nav-move-line-or-region-down)
         ("C-M-<left>" . elpy-nave-indent-shift-left)
         ("C-M-<right>" . elpy-nave-indent-shift-right))
  )
(general-unbind
  :keymaps 'elpy-mode-map
  "M-<up>"
  "M-<down>"
  "M-<left>"
  "M-<right>")

(setq elpy-rpc-backend "jedi")
(elpy-enable)


;;---===>>><<<===---
;;        C
;;---===>>><<<===---

;; Language server : ccls
(use-package ccls
  :hook ((c-mode c++-mode objc-mode cuda-mode) .
         (lambda () (require 'ccls) (lsp))))

;;---===>>><<<===---
;;       JSON
;;---===>>><<<===---

(use-package json-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.json\\'" . json-mode)))


;;---===>>><<<===---
;;      LaTeX
;;---===>>><<<===---

(use-package pdf-tools
  :ensure t
  :config
  (pdf-tools-install))



;;---===>>><<<===---
;;    Keybindings
;;---===>>><<<===---

(general-define-key
 "C-c C-r" 'quickrun-shell              ; compile and run for any language (almost)
 "C-c C-p" 'build-and-run               ; build and run Gradle project
 "C-c C-v" 'lsp-execute-code-action     ; execute contextual code action
 "C-c C-b" 'company-complete            ; trigger autocompletion
 "C-<tab>" 'origami-forward-toggle-node ; fold/unflod code block
 )


;;#########################
;;                        #
;;        ORG-MODE        #
;;                        #
;;#########################

;; What we want to be executed when an org buffer is started
(defun vs/org-mode-setup ()
  (org-indent-mode)
  )

(use-package org
  :config(setq org-ellipsis " »"))


;;#########################
;;                        #
;;      MISCELLANEOUS     #
;;                        #
;;#########################

;; Discord rich presence
(use-package elcord)
(elcord-mode)

;; Activity Watch ! To track my emacs time !
(use-package activity-watch-mode)
(global-activity-watch-mode)

