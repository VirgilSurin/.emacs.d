;; No startup message. Thanks but no thanks.
(setq inhibit-startup-message t)

;; Visual clean-up

(scroll-bar-mode -1) ; Disable scrollbar
(tool-bar-mode -1)   ; Disable toolbar
(tooltip-mode -1)    ; Disable tooltip
(set-fringe-mode 10) ; Give some space
(menu-bar-mode -1)   ; Disable the menu bar

;; Typo
(set-face-attribute 'default nil :font "Fira Code Retina" :height 100)

;; enable line highlight, line number and column number
(global-hl-line-mode 0) ; Disabled for now. Theme does not provide a nice highlight
(global-display-line-numbers-mode t)
;; Disable line mode for some modes
(dolist (mode '(org-mode-hook
		term-mode-hook
		shell-mode-hook
		eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; Theme
(load-theme 'wombat)

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; Initialize package sources
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			 ("melpa-stable" . "https://stable.melpa.org/packages/")
			 ("org" . "htps://orgmode.org/elpa/")
			 ("elpa" . "https://elpa.gu.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(use-package command-log-mode) ;; shows what I press (basically)

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

(use-package counsel)

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 15)))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

