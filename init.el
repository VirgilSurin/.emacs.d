


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

;; Custom function to toggle transparency mode
(defun toggle-transparency (n)
  "toggle transparency"
  (interactive "nValue: ")
  (set-frame-parameter (selected-frame) 'alpha '(n . n))
  (add-to-list 'default-frame-alist '(alpha . (n . n))))



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

;; Benchmark Emacs start-up time :)
(unless (package-installed-p 'esup)
    (package-refresh-contents)
    (package-install 'esup))


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
;;    IDE/CODE SETUP      #
;;                        #
;;#########################

;; Ocaml
(use-package tuareg)



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
