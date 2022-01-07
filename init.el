
(setq inhibit-startup-message t)
(tooltip-mode -1)
(setq visible-bell t)
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(global-display-line-numbers-mode t)
(electric-pair-mode t)
(show-paren-mode 1)
(delete-selection-mode 1)
(setq blink-cursor-blinks 0)
(setq-default cursor-type 'bar)
(setq c-default-style
      '((java-mode . "linux")))
(setq mouse-wheel-progressive-speed nil)

(set-default-coding-systems 'utf-8)

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

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(load-theme 'atom-one-dark t)

;; Taken from -> https://tsdh.org/posts/2021-06-06-update-all-emacs-packages-from-the-command-line.html
(use-package auto-package-update
  :ensure t
  :config
  ;; Yes, please delete the old version on updates.
  (setq auto-package-update-delete-old-versions t))

(set-face-attribute 'default nil :font "Fira Code Medium" :height 90)
;; (set-face-attribute 'default nil :font "Consolas" :height 90)

;; Package for usint M-up and M-down to move lines around
(use-package move-dup)

;; Rust config
(use-package rust-mode)
(use-package rustic)
(setq lsp-rust-analyzer-server-display-inlay-hints t)
(setq lsp-rust-server 'rust-analyzer)

;Disable line numbers for following modes
(dolist (mode '(org-mode-hook
		term-mode-hook
		eshell-mode-hook
		shell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; Expand region (C +)
(use-package expand-region)

;;Rainbow Delimiters
(use-package rainbow-delimiters
  :ensure t
  :init
  (progn
    (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)))

;; All-the-icons
(use-package all-the-icons
  :init)

;; Doom-Modeline
(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))

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
(use-package company-quickhelp)

(global-company-mode 1)
(company-quickhelp-mode)

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

(use-package counsel-tramp)

;; LSP-Mode
(use-package lsp-mode
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l")
  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
         (java-mode . lsp)
         ;; if you want which-key integration
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp
  :config
  (lsp-treemacs-sync-mode 1))
(use-package lsp-ui)
(setq lsp-ui-sideline-delay 0)
;; to fix weird semicolon line deletion issue
(setq lsp-java-format-on-type-enabled nil)
(setq lsp-enable-on-type-formatting nil)
;;(setq lsp-ui-sideline-show-code-actions t)

(use-package projectile)
(use-package flycheck :ensure)
(use-package yasnippet :config (yas-global-mode))

;; Magit
(use-package magit
  :bind ("C-x g" . magit-status))
(use-package ssh-agency)

;; IDE-type Config
(use-package lsp-ivy :commands lsp-ivy-workspace-symbol)
(use-package lsp-treemacs :commands lsp-treemacs-errors-list)
(use-package lsp-java :config (add-hook 'java-mode-hook 'lsp))
(use-package dap-mode :after lsp-mode :config (dap-auto-configure-mode))
(use-package dap-java :ensure nil)
(use-package java-snippets)
(use-package lsp-python-ms
  :ensure t
  :init (setq lsp-python-ms-auto-install-server t)
  :hook (python-mode . (lambda ()
                          (require 'lsp-python-ms)
                          (lsp))))
(setq lsp-ui-doc-show-with-cursor nil)
(setq treemacs-python-executable "C:\\Program Files (x86)\\Microsoft Visual Studio\\Shared\\Python37_64\\python.exe")

;Which-key
(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 1.0))

;; Key Bindings

;; Move Dup (moving lines like vscode)
(global-set-key (kbd "M-<up>") 'move-dup-move-lines-up)
(global-set-key (kbd "M-<down>") 'move-dup-move-lines-down)
(global-set-key (kbd "C-M-<up>") 'move-dup-duplicate-up)
(global-set-key (kbd "C-M-<down>") 'move-dup-duplicate-down)

;: Window Resize
(global-set-key (kbd "S-C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<down>") 'shrink-window)
(global-set-key (kbd "S-C-<up>") 'enlarge-window)

;: Ivy/Counsel/Swiper
(global-set-key (kbd "C-s") 'swiper-isearch)
(global-set-key (kbd "C-r") 'swiper-isearch-backward)
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

;: Expand Region
(global-set-key (kbd "C-=") 'er/expand-region)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#21252B" "#E06C75" "#98C379" "#E5C07B" "#61AFEF" "#C678DD" "#56B6C2" "#ABB2BF"])
 '(company-quickhelp-color-background "#3E4452")
 '(company-quickhelp-color-foreground "#ABB2BF")
 '(compilation-message-face 'default)
 '(custom-safe-themes
   '("5b7c31eb904d50c470ce264318f41b3bbc85545e4359e6b7d48ee88a892b1915" "3e5c1261d06395a74566da3af413ab909a2049e0c52d9297a5e2d6823bf189d6" "44546a3c5032ace263613f39669a66c604523001135f5b42ea583c9abf6f0a5e" "493434ed95de30b7648c293fc482c0bb3e3ba95543bdd936d89490da0ef5ebd5" "6c12baea488aa868d32ee648fd5a704a351dd4d8689da6e21459200d386eaab1" "7affd7ecac23d6f5f43f115d2ce3f9a95f1d65c2da6cedddeae53ecf1b7470f3" "3bea339b9d83c48cddd1080494bc4971b7a85a78c51545fcf1429e7838f8b918" "aaceba7dd433b4eed1de887b5c72a53f014237042704a441066e933235a5ab3a" "12db058ce4ba460e067e331a67dbb05c4406d8c0d5e4504cebc059cffae55672" "af9e9a92b17bb6f50d623867e1da6db47e015c30091f325dbe473abe7b397ba4" "a31ca6382a13b79c63f7cfbf535099b73c0496837dc255b7158d3836488739db" "3db307fb06cedec4f2f6dfcbc189ffc26bca9653d7e149643d451b8411a8f039" "0c860c4fe9df8cff6484c54d2ae263f19d935e4ff57019999edbda9c7eda50b8" "ebbd4bbb0f017cb09f7a3b1363b83dfde0c5f4970cda2705419457366cd2de91" default))
 '(electric-pair-mode t)
 '(fci-rule-color "#3E4451")
 '(highlight-changes-colors '("#ff8eff" "#ab7eff"))
 '(highlight-tail-colors
   '(("#323342" . 0)
     ("#63de5d" . 20)
     ("#4BBEAE" . 30)
     ("#1DB4D0" . 50)
     ("#9A8F21" . 60)
     ("#A75B00" . 70)
     ("#F309DF" . 85)
     ("#323342" . 100)))
 '(hl-sexp-background-color "#1c1f26")
 '(magit-diff-use-overlays nil)
 '(package-selected-packages
   '(expand-region atom-dark-theme auto-package-update rust-mode move-dup counsel-tramp ssh-agency magit doom-modeline lsp-python-ms all-the-icons java-snippets yasnippet projectile flycheck lsp-python lsp-java dap-python dap-java dap-mode lsp-treemacs lsp-ivy lsp-ui lsp-mode company ivy-rich which-key use-package rainbow-delimiters popup javaimp counsel async))
 '(pos-tip-background-color "#E6DB74")
 '(pos-tip-foreground-color "#242728")
 '(tetris-x-colors
   [[229 192 123]
    [97 175 239]
    [209 154 102]
    [224 108 117]
    [152 195 121]
    [198 120 221]
    [86 182 194]])
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   '((20 . "#ff0066")
     (40 . "#CF4F1F")
     (60 . "#C26C0F")
     (80 . "#E6DB74")
     (100 . "#AB8C00")
     (120 . "#A18F00")
     (140 . "#989200")
     (160 . "#8E9500")
     (180 . "#63de5d")
     (200 . "#729A1E")
     (220 . "#609C3C")
     (240 . "#4E9D5B")
     (260 . "#3C9F79")
     (280 . "#53f2dc")
     (300 . "#299BA6")
     (320 . "#2896B5")
     (340 . "#2790C3")
     (360 . "#06d8ff")))
 '(vc-annotate-very-old-color nil)
 '(weechat-color-list
   (unspecified "#242728" "#323342" "#F70057" "#ff0066" "#86C30D" "#63de5d" "#BEB244" "#E6DB74" "#40CAE4" "#06d8ff" "#FF61FF" "#ff8eff" "#00b2ac" "#53f2dc" "#f8fbfc" "#ffffff")))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(font-lock-comment-face ((t (:foreground "#5C6370" :slant italic)))))
