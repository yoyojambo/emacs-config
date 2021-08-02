
(setq inhibit-startup-message t)
(tooltip-mode -1)
(setq visible-bell t)
(tool-bar-mode -1)
(global-display-line-numbers-mode t)
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(load-theme 'atom-one-dark t)
(electric-pair-mode)
(delete-selection-mode 1)
(setq blink-cursor-blinks 0)
(setq-default cursor-type 'bar)
(setq c-default-style
      '((java-mode . "linux")))

(require 'package)

(setq package-archives '(("org" . "https://orgmode.org/elpa/")
			 ("elpa" . "https://elpa.gnu.org/packages/")))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(set-face-attribute 'default nil :font "Fira Code Medium" :height 90)
;; (set-face-attribute 'default nil :font "Consolas" :height 90)

;Disable line numbers for following modes
(dolist (mode '(org-mode-hook
		term-mode-hook
		eshell-mode-hook
		shell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))


;;Rainbow Delimiters
(use-package rainbow-delimiters
  :ensure t
  :init
  (progn
    (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)))

;; All-the-icons
(use-package all-the-icons
  :init)

;Company Mode
(use-package company
  :ensure t
  :init
  (setq company-show-numbers t)       ; visual numbering of candidates
  :config
  (add-hook 'after-init-hook 'global-company-mode)
  :bind
  (:map company-active-map ("<tab>" . company-complete-selection)))
(setq company-idle-delay 0)
(setq company-minimum-prefix-length 1)

;Ivy
(use-package ivy
  :diminish
  :config
  (ivy-mode 1))
(setq ivy-use-virtual-buffers t)
(setq ivy-count-format "(%d/%d) ")

(use-package ivy-rich
  :init
  (ivy-rich-mode 1))

;; LSP-Mode
(use-package lsp-mode
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l")
  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
         (java-mode . lsp)
         ;; if you want which-key integration
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp)

(use-package projectile)
(use-package lsp-ui)
(setq lsp-ui-sideline-delay 0)
(use-package flycheck)
(use-package yasnippet :config (yas-global-mode))
(use-package java-snippets)

;; IDE-type Config
(use-package lsp-ivy :commands lsp-ivy-workspace-symbol)
(use-package lsp-treemacs :commands lsp-treemacs-errors-list)
(use-package lsp-java :config (add-hook 'java-mode-hook 'lsp))
(setq lsp-ui-doc-show-with-cursor nil)
(use-package dap-mode :after lsp-mode :config (dap-auto-configure-mode))
(use-package dap-java :ensure nil)
(use-package lsp-python-ms
  :ensure t
  :init (setq lsp-python-ms-auto-install-server t)
  :hook (python-mode . (lambda ()
                          (require 'lsp-python-ms)
                          (lsp))))


(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 1.0))

;Key Bindings

;Window Resize
(global-set-key (kbd "S-C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<down>") 'shrink-window)
(global-set-key (kbd "S-C-<up>") 'enlarge-window)

; Ivy/Counsel/Swiper
(global-set-key (kbd "C-s") 'swiper-isearch)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "M-y") 'counsel-yank-pop)
(global-set-key (kbd "<f1> f") 'counsel-describe-function)
(global-set-key (kbd "<f1> v") 'counsel-describe-variable)
(global-set-key (kbd "<f1> l") 'counsel-find-library)
(global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
(global-set-key (kbd "<f2> u") 'counsel-unicode-char)
(global-set-key (kbd "<f2> j") 'counsel-set-variable)
(global-set-key (kbd "C-x b") 'ivy-switch-buffer)
(global-set-key (kbd "C-c v") 'ivy-push-view)
(global-set-key (kbd "C-c V") 'ivy-pop-view)



(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#21252B" "#E06C75" "#98C379" "#E5C07B" "#61AFEF" "#C678DD" "#56B6C2" "#ABB2BF"])
 '(custom-safe-themes
   '("0c860c4fe9df8cff6484c54d2ae263f19d935e4ff57019999edbda9c7eda50b8" "ebbd4bbb0f017cb09f7a3b1363b83dfde0c5f4970cda2705419457366cd2de91" default))
 '(electric-pair-mode t)
 '(fci-rule-color "#3E4451")
 '(package-selected-packages
   '(lsp-python-ms all-the-icons java-snippets yasnippet projectile flycheck lsp-python lsp-java dap-python dap-java dap-mode lsp-treemacs lsp-ivy lsp-ui lsp-mode company ivy-rich which-key use-package rainbow-delimiters popup javaimp counsel async))
 '(tetris-x-colors
   [[229 192 123]
    [97 175 239]
    [209 154 102]
    [224 108 117]
    [152 195 121]
    [198 120 221]
    [86 182 194]]))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
