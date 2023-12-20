
(setq inhibit-startup-message t)
(tooltip-mode -1)
(setq visible-bell t)
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(column-number-mode 1)
(setq frame-resize-pixelwise 1)
(electric-pair-mode t)
(show-paren-mode 1)
(delete-selection-mode 1)
(cd "~/")

(global-display-line-numbers-mode t)
(setq blink-cursor-blinks 0)
(setq-default cursor-type 'bar)

(setq c-default-style
      '((java-mode . "linux")))
(setq mouse-wheel-progressive-speed nil)

(set-default-coding-systems 'utf-8)

;; To make backups (file.extension~) not clutter every directory.
(setq backup-directory-alist
          `((".*" . ,temporary-file-directory)))
    (setq auto-save-file-name-transforms
          `((".*" ,temporary-file-directory t)))

(require 'package)

(setq package-archives '(("org" . "https://orgmode.org/elpa/")
			 ("elpa" . "https://elpa.gnu.org/packages/")))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

;; Nomas para el startup
(setq gc-cons-threshold most-positive-fixnum)

;; Lower threshold back to 8 MiB (default is 800kB)
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (expt 2 23))))

(package-initialize)
(setq package-enable-at-startup nil)
(unless package-archive-contents
  (package-refresh-contents))
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)
(setq use-package-compute-statistics t)

(use-package doom-themes
  :config
  ;; Global settings
  (setq doom-themes-enable-bold t
	doom-themes-enable-italic t))

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(load-theme 'doom-one t)

;; Taken from -> https://tsdh.org/posts/2021-06-06-update-all-emacs-packages-from-the-command-line.html
(use-package auto-package-update
  :ensure t
  :config
  ;; Yes, please delete the old version on updates.
  (setq auto-package-update-delete-old-versions t))

;; Font
(set-face-attribute 'default nil :font "Fira Code" :height 90)

;; Ligatures TRADICIONAL
(use-package ligature
  :config
  (ligature-set-ligatures 'prog-mode
                        '(;; == === ==== => =| =>>=>=|=>==>> ==< =/=//=// =~
                          ;; =:= =!=
                          ("=" (rx (+ (or ">" "<" "|" "/" "~" ":" "!" "="))))
                          ;; ;; ;;;
                          (";" (rx (+ ";")))
                          ;; && &&&
                          ("&" (rx (+ "&")))
                          ;; !! !!! !. !: !!. != !== !~
                          ("!" (rx (+ (or "=" "!" "\." ":" "~"))))
                          ;; ?? ??? ?:  ?=  ?.
                          ("?" (rx (or ":" "=" "\." (+ "?"))))
                          ;; %% %%%
                          ("%" (rx (+ "%")))
                          ;; |> ||> |||> ||||> |] |} || ||| |-> ||-||
                          ;; |->>-||-<<-| |- |== ||=||
                          ;; |==>>==<<==<=>==//==/=!==:===>
                          ("|" (rx (+ (or ">" "<" "|" "/" ":" "!" "}" "\]"
                                          "-" "=" ))))
                          ;; \\ \\\ \/
                          ("\\" (rx (or "/" (+ "\\"))))
                          ;; ++ +++ ++++ +>
                          ("+" (rx (or ">" (+ "+"))))
                          ;; :: ::: :::: :> :< := :// ::=
                          (":" (rx (or ">" "<" "=" "//" ":=" (+ ":"))))
                          ;; // /// //// /\ /* /> /===:===!=//===>>==>==/
                          ("/" (rx (+ (or ">"  "<" "|" "/" "\\" "\*" ":" "!"
                                          "="))))
                          ;; .. ... .... .= .- .? ..= ..<
                          ("\." (rx (or "=" "-" "\?" "\.=" "\.<" (+ "\."))))
                          ;; -- --- ---- -~ -> ->> -| -|->-->>->--<<-|
                          ("-" (rx (+ (or ">" "<" "|" "~" "-"))))
                          ;; *> */ *)  ** *** ****
                          ("*" (rx (or ">" "/" ")" (+ "*"))))
                          ;; www wwww
                          ("w" (rx (+ "w")))
                          ;; <> <!-- <|> <: <~ <~> <~~ <+ <* <$ </  <+> <*>
                          ;; <$> </> <|  <||  <||| <|||| <- <-| <-<<-|-> <->>
                          ;; <<-> <= <=> <<==<<==>=|=>==/==//=!==:=>
                          ;; << <<< <<<<
                          ("<" (rx (+ (or "\+" "\*" "\$" "<" ">" ":" "~"  "!"
                                          "-"  "/" "|" "="))))
                          ;; >: >- >>- >--|-> >>-|-> >= >== >>== >=|=:=>>
                          ;; >> >>> >>>>
                          (">" (rx (+ (or ">" "<" "|" "/" ":" "=" "-"))))
                          ;; #: #= #! #( #? #[ #{ #_ #_( ## ### ##### 
                          ("#" (rx (or ":" "=" "!" "(" "\?" "\[" "{" "_(" "_"
                                       (+ "#"))))
                          ;; ~~ ~~~ ~=  ~-  ~@ ~> ~~>
                          ("~" (rx (or ">" "=" "-" "@" "~>" (+ "~"))))
                          ;; __ ___ ____ _|_ __|____|_
                          ("_" (rx (+ (or "_" "|"))))
                          ;; Fira code: 0xFF 0x12
                          ("0" (rx (and "x" (+ (in "A-F" "a-f" "0-9")))))
                          ;; Fira code:
                          "Fl"  "Tl"  "fi"  "fj"  "fl"  "ft"
                          ;; The few not covered by the regexps.
                          "{|"  "[|"  "]#"  "(*"  "}#"  "$>"  "^=")))

;; (use-package ligature
;;   :config
;;   (ligature-set-ligatures 'prog-mode '("|||>" "<|||" "<==>" "<!--" "####" "~~>" "***" "||=" "||>"
;;                                        ":::" "::=" "=:=" "===" "==>" "=!=" "=>>" "=<<" "=/=" "!=="
;;                                        "!!." ">=>" ">>=" ">>>" ">>-" ">->" "->>" "-->" "---" "-<<"
;;                                        "<~~" "<~>" "<*>" "<||" "<|>" "<$>" "<==" "<=>" "<=<" "<->"
;;                                        "<--" "<-<" "<<=" "<<-" "<<<" "<+>" "</>" "###" "#_(" "..<"
;;                                        "..." "+++" "/==" "///" "_|_" "&&" "^=" "~~" "~@" "~=" ; "www" eliminated
;;                                        "~>" "~-" "**" "*>" "*/" "||" "|}" "|]" "|=" "|>" "|-" "{|"
;;                                        "[|" "]#" "::" ":=" ":>" ":<" "$>" "==" "=>" "!=" "!!" ">:"
;;                                        ">=" ">>" ">-" "-~" "-|" "->" "--" "-<" "<~" "<*" "<|" "<:"
;;                                        "<$" "<=" "<>" "<-" "<<" "<+" "</" "#{" "#[" "#:" "#=" "#!"
;;                                        "##" "#(" "#?" "#_" "%%" ".=" ".-" ".." ".?" "+>" "++" "?:"
;;                                        "?=" "?." "??" ";;" "/*" "/=" "/>" "//" "__" "~~" "(*" "*)"
;;                                        "\\\\" "://" "!~" "::>" "<::" "#####" "######" "----")))

(global-ligature-mode 1)

;; No se necesita gracias a ligature.el
;; ;; fira-code-mode
;; (use-package fira-code-mode
;;   :config (global-fira-code-mode))

;; Original ligature set from https://github.com/mickeynp/ligature.el#readme
;; '("|||>" "<|||" "<==>" "<!--" "####" "~~>" "***" "||=" "||>"
;;   ":::" "::=" "=:=" "===" "==>" "=!=" "=>>" "=<<" "=/=" "!=="
;;   "!!." ">=>" ">>=" ">>>" ">>-" ">->" "->>" "-->" "---" "-<<"
;;   "<~~" "<~>" "<*>" "<||" "<|>" "<$>" "<==" "<=>" "<=<" "<->"
;;   "<--" "<-<" "<<=" "<<-" "<<<" "<+>" "</>" "###" "#_(" "..<"
;;   "..." "+++" "/==" "///" "_|_" "www" "&&" "^=" "~~" "~@" "~="
;;   "~>" "~-" "**" "*>" "*/" "||" "|}" "|]" "|=" "|>" "|-" "{|"
;;   "[|" "]#" "::" ":=" ":>" ":<" "$>" "==" "=>" "!=" "!!" ">:"
;;   ">=" ">>" ">-" "-~" "-|" "->" "--" "-<" "<~" "<*" "<|" "<:"
;;   "<$" "<=" "<>" "<-" "<<" "<+" "</" "#{" "#[" "#:" "#=" "#!"
;;   "##" "#(" "#?" "#_" "%%" ".=" ".-" ".." ".?" "+>" "++" "?:"
;;   "?=" "?." "??" ";;" "/*" "/=" "/>" "//" "__" "~~" "(*" "*)"
;;   "\\\\" "://")

;; Tree-sitter
(setq treesit-language-source-alist
   '((bash "https://github.com/tree-sitter/tree-sitter-bash")
     (cmake "https://github.com/uyha/tree-sitter-cmake")
     (css "https://github.com/tree-sitter/tree-sitter-css")
     (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
     (elisp "https://github.com/Wilfred/tree-sitter-elisp")
     (go "https://github.com/tree-sitter/tree-sitter-go")
     (html "https://github.com/tree-sitter/tree-sitter-html")
     (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
     (json "https://github.com/tree-sitter/tree-sitter-json")
     (make "https://github.com/alemuller/tree-sitter-make")
     (markdown "https://github.com/ikatyang/tree-sitter-markdown")
     (python "https://github.com/tree-sitter/tree-sitter-python")
     (toml "https://github.com/tree-sitter/tree-sitter-toml")
     (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
     (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
     (yaml "https://github.com/ikatyang/tree-sitter-yaml")))

(use-package google-c-style
  :hook (c++-mode . google-set-c-style))

;; Package for usint M-up and M-down to move lines around
(use-package move-dup)

;; Nim config
(use-package nim-mode
  :mode "\\.nim\\'"

  :config
  ;(add-to-list 'eglot-server-programs '(nim-mode "nimlsp"))
  ;; :config
  ;; (progn (define-key nim-mode-map (kbd "C-c C-c") nil)
  ;;         (define-key nim-mode-map (kbd "C-c <") nil)
  ;; 	  (define-key nim-mode-map (kbd "C-c >") nil))
  )

;; Rust config
;; (use-package rust-mode
;;   :mode )
;; (use-package rustic
;;   :mode "\\.rs\\'"
;;   :config
;;   (setq lsp-rust-analyzer-server-display-inlay-hints t)
;;   (setq lsp-rust-server 'rust-analyzer)
;; )

;; Plantuml-mode
(use-package plantuml-mode
  :mode "\\.puml\\'"
  :config (setq plantuml-default-exec-mode 'jar))

;; Org mode
(add-hook 'org-mode-hook (lambda () (org-indent-mode)))

;; PDF-tools
;; (use-package pdf-tools
;;   :pin manual
;;   :mode ("\\.pdf\\'")
;;   :config
;;   (pdf-tools-install)
;;   (setq-default pdf-view-display-size 'fit-height)
;;   (define-key pdf-view-mode-map (kbd "C-s") 'isearch-forward))

;; https://github.com/jwiegley/use-package#magic-handlers
(use-package pdf-tools
  :load-path "site-lisp/pdf-tools/lisp"
  :magic ("%PDF" . pdf-view-mode)
  :config
  (pdf-tools-install :no-query)
  (setq-default pdf-view-display-size 'fit-height)
  (define-key pdf-view-mode-map (kbd "C-s") 'isearch-forward))

(use-package org-modern
  :hook (org-mode . org-modern-mode))

;; Disable line numbers for following modes
(dolist
    (mode-hook
     '(org-mode-hook
       term-mode-hook
       eshell-mode-hook
       shell-mode-hook
       dired-mode-hook
       image-mode-hook
       pdf-view-mode-hook))
  (add-hook mode-hook (lambda () (display-line-numbers-mode 0))))

;; Eshell prompt colors
(defun my-eshell-prompt ()
  "Highlight eshell pwd and prompt separately."
  (mapconcat
   (lambda (list)
     (propertize (car list)
                 'read-only      t
                 'font-lock-face (cdr list)
                 'front-sticky   '(font-lock-face read-only)
                 'rear-nonsticky '(font-lock-face read-only)))
   `((,(abbreviate-file-name (eshell/pwd)) :foreground "dodger blue")
     (,(if (zerop (user-uid)) " # " " $ ") :foreground "orchid"))
   ""))

(setq eshell-highlight-prompt nil
      eshell-prompt-function  #'my-eshell-prompt)

;; Expand region (C-+)
(use-package expand-region)

;; Rainbow Delimiters
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
  :demand t
  :config (doom-modeline-mode 1))

;; Company Mode
(use-package company
  :ensure t
  :init
  (setq company-show-numbers t)       ; visual numbering of candidates
  :config
  (progn (add-hook 'after-init-hook 'global-company-mode)
	 ;; These two are so that hitting return will never select a completion.
	 (define-key company-active-map (kbd "<return>") nil)
	 (define-key company-active-map (kbd "RET") nil))
  :custom
  (company-idle-delay 0 "Instant completion always.")
  (company-minimum-prefix-length 1 "Only need to type a single character to provide completion.")
  :bind
  (:map company-active-map ("<tab>" . company-complete-selection))
  )

(use-package company-quickhelp)

;; With use-package:
;; (use-package company-box
;;   :hook (company-mode . company-box-mode))

(global-company-mode 1)
(company-quickhelp-mode)

;; Disable company-mode in terminals
(dolist (mode '(shell-mode-hook
		term-mode-hook
		eshell-mode-hook))
  (add-hook mode (lambda () (company-mode 0))))
(setq company-dabbrev-downcase nil)

;; Ivy
(use-package ivy
  :diminish
  :config
  (ivy-mode 1)
  :custom
  (ivy-use-virtual-buffers t)
  (ivy-count-format "(%d/%d) " "Sets the format around the option counter in ivy, counsel, swiper.")  
)

(use-package ivy-rich
  :init
  (ivy-rich-mode 1))

;; (use-package counsel-tramp)

;; nerd-icons
(use-package nerd-icons)

(use-package nerd-icons-dired
  :hook
  (dired-mode . nerd-icons-dired-mode))

(use-package nerd-icons-ivy-rich
  :ensure t
  :init
  (nerd-icons-ivy-rich-mode 1))

(use-package nerd-icons-completion
  :config
  (nerd-icons-completion-mode))

;; LSP-Mode
(use-package lsp-mode
  :hook (lsp-mode . yas-minor-mode-on)
  :config (use-package lsp-ui))

;; WARNING: Never add values to 'lsp-enabled-clients', rather add undesired clients to 'lsp-disabled-clients'.
;; any clients not found in 'lsp-enabled-clients' will show an error message when trying to load them.
;; (use-package lsp-mode
;;   :init
;;   ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
;;   (setq lsp-keymap-prefix "C-c l")
;;   :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
;;          ;(java-mode . lsp)
;;          ;; if you want which-key integration
;;          (lsp-mode . lsp-enable-which-key-integration))
;;   :commands lsp
;;   :config
;;   (lsp-treemacs-sync-mode 1)
;;   (defun lsp--clean-company ()
;;     (remove-hook 'company-completion-started-hook
;; 		 (lambda (&rest _)
;;                    (lsp--capf-clear-cache)
;;                    (setq-local lsp-inhibit-lsp-hooks t))
;; 		 t)
;;     (remove-hook 'company-after-completion-hook
;; 		 (lambda (&rest _)
;;                    (lsp--capf-clear-cache)
;;                    (setq-local lsp-inhibit-lsp-hooks nil))
;; 		 t)))
;; (use-package lsp-ui)
;; (setq lsp-ui-sideline-delay 0)
;; ;(setq lsp-java-format-on-type-enabled nil) ;; to fix weird semicolon line deletion issue
;; (setq lsp-enable-on-type-formatting nil)
;; ;;(setq lsp-ui-sideline-show-code-actions t)

(use-package projectile)
(use-package flycheck :ensure)
(use-package flycheck-nim)

;; (use-package yasnippet
;;   :ensure t
;;   :config
;;   (use-package yasnippet-snippets
;;     :ensure t)
;;   ;(setq company-backends '((:separate company-yasnippet company-capf))
;;   (global-set-key (kbd "<M-return>") 'yas-expand)
;;   (yas-reload-all)
;;   )

(yas-global-mode 1)

(use-package yasnippet
  :hook (prog-mode . yas-minor-mode)
  :config
  (global-set-key (kbd "<M-return>") 'yas-expand)
  ;(yas-reload-all)
)

(use-package yasnippet-snippets
  :after (yasnippet))

;; Magit
(use-package magit
  :bind ("C-x g" . magit-status))

(use-package ssh-agency)

;; IDE-type Config
;; (use-package lsp-ivy :commands lsp-ivy-workspace-symbol)
;; (use-package lsp-treemacs :commands lsp-treemacs-errors-list)

;; (use-package lsp-java
;;   :defer
;;   :config
;;   (add-hook 'java-mode-hook 'lsp))
;(use-package dap-mode :after lsp-mode :config (dap-auto-configure-mode))
;; (use-package dap-java
;;   :defer
;;   :ensure nil)
;; (use-package java-snippets)
;; (use-package lsp-python-ms
;;   :ensure t
;;   :init (setq lsp-python-ms-auto-install-server t)
;;   :hook (python-mode . (lambda ()
;;                           (require 'lsp-python-ms)
;;                           (lsp))))
;; (use-package lsp-jedi
;;   :ensure t)
;; (setq lsp-ui-doc-show-with-cursor nil)

;; fic-mode for highlighting of TODO comments
;; (use-package fic-mode)
;; (add-hook 'nim-mode-hook 'fic-mode)

;Which-key
(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 1.0))


;; Key Bindings

(defun newline-without-break-of-line ()
  ;; Newline without breaking current line
  ;;  1. move to end of the line.
  ;;  2. insert newline with index
  (interactive)
  (let ((oldpos (point)))
    (end-of-line)
    (newline-and-indent)))

;; QOL tweaks
(global-set-key (kbd "<C-return>") 'newline-without-break-of-line)
(global-set-key (kbd "C-<tab>") 'beginning-of-line-text)

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
 '(company-box-frame-behavior 'point)
 '(company-quickhelp-color-background "#3E4452")
 '(company-quickhelp-color-foreground "#ABB2BF")
 '(compilation-message-face 'default)
 '(connection-local-criteria-alist
   '(((:application eshell)
      eshell-connection-default-profile)
     ((:application tramp)
      tramp-connection-local-default-system-profile tramp-connection-local-default-shell-profile)))
 '(connection-local-profile-alist
   '((eshell-connection-default-profile
      (eshell-path-env-list))
     (tramp-connection-local-darwin-ps-profile
      (tramp-process-attributes-ps-args "-acxww" "-o" "pid,uid,user,gid,comm=abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ" "-o" "state=abcde" "-o" "ppid,pgid,sess,tty,tpgid,minflt,majflt,time,pri,nice,vsz,rss,etime,pcpu,pmem,args")
      (tramp-process-attributes-ps-format
       (pid . number)
       (euid . number)
       (user . string)
       (egid . number)
       (comm . 52)
       (state . 5)
       (ppid . number)
       (pgrp . number)
       (sess . number)
       (ttname . string)
       (tpgid . number)
       (minflt . number)
       (majflt . number)
       (time . tramp-ps-time)
       (pri . number)
       (nice . number)
       (vsize . number)
       (rss . number)
       (etime . tramp-ps-time)
       (pcpu . number)
       (pmem . number)
       (args)))
     (tramp-connection-local-busybox-ps-profile
      (tramp-process-attributes-ps-args "-o" "pid,user,group,comm=abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ" "-o" "stat=abcde" "-o" "ppid,pgid,tty,time,nice,etime,args")
      (tramp-process-attributes-ps-format
       (pid . number)
       (user . string)
       (group . string)
       (comm . 52)
       (state . 5)
       (ppid . number)
       (pgrp . number)
       (ttname . string)
       (time . tramp-ps-time)
       (nice . number)
       (etime . tramp-ps-time)
       (args)))
     (tramp-connection-local-bsd-ps-profile
      (tramp-process-attributes-ps-args "-acxww" "-o" "pid,euid,user,egid,egroup,comm=abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ" "-o" "state,ppid,pgid,sid,tty,tpgid,minflt,majflt,time,pri,nice,vsz,rss,etimes,pcpu,pmem,args")
      (tramp-process-attributes-ps-format
       (pid . number)
       (euid . number)
       (user . string)
       (egid . number)
       (group . string)
       (comm . 52)
       (state . string)
       (ppid . number)
       (pgrp . number)
       (sess . number)
       (ttname . string)
       (tpgid . number)
       (minflt . number)
       (majflt . number)
       (time . tramp-ps-time)
       (pri . number)
       (nice . number)
       (vsize . number)
       (rss . number)
       (etime . number)
       (pcpu . number)
       (pmem . number)
       (args)))
     (tramp-connection-local-default-shell-profile
      (shell-file-name . "/bin/sh")
      (shell-command-switch . "-c"))
     (tramp-connection-local-default-system-profile
      (path-separator . ":")
      (null-device . "/dev/null"))))
 '(custom-safe-themes
   '("eca44f32ae038d7a50ce9c00693b8986f4ab625d5f2b4485e20f22c47f2634ae" "02f57ef0a20b7f61adce51445b68b2a7e832648ce2e7efb19d217b6454c1b644" "5b7c31eb904d50c470ce264318f41b3bbc85545e4359e6b7d48ee88a892b1915" "3e5c1261d06395a74566da3af413ab909a2049e0c52d9297a5e2d6823bf189d6" "44546a3c5032ace263613f39669a66c604523001135f5b42ea583c9abf6f0a5e" "493434ed95de30b7648c293fc482c0bb3e3ba95543bdd936d89490da0ef5ebd5" "6c12baea488aa868d32ee648fd5a704a351dd4d8689da6e21459200d386eaab1" "7affd7ecac23d6f5f43f115d2ce3f9a95f1d65c2da6cedddeae53ecf1b7470f3" "3bea339b9d83c48cddd1080494bc4971b7a85a78c51545fcf1429e7838f8b918" "aaceba7dd433b4eed1de887b5c72a53f014237042704a441066e933235a5ab3a" "12db058ce4ba460e067e331a67dbb05c4406d8c0d5e4504cebc059cffae55672" "af9e9a92b17bb6f50d623867e1da6db47e015c30091f325dbe473abe7b397ba4" "a31ca6382a13b79c63f7cfbf535099b73c0496837dc255b7158d3836488739db" "3db307fb06cedec4f2f6dfcbc189ffc26bca9653d7e149643d451b8411a8f039" "0c860c4fe9df8cff6484c54d2ae263f19d935e4ff57019999edbda9c7eda50b8" "ebbd4bbb0f017cb09f7a3b1363b83dfde0c5f4970cda2705419457366cd2de91" default))
 '(doc-view-continuous t)
 '(doom-modeline-time-icon nil)
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
 '(lsp-clangd-binary-path "/usr/bin/clang")
 '(lsp-clangd-version "14.0.0")
 '(lsp-clients-clangd-executable "/usr/bin/clangd")
 '(magit-diff-use-overlays nil)
 '(org-agenda-files '("/home/yoyojambo/OneDrive/Agenda.org" "~/Personal.org"))
 '(org-export-backends '(ascii html icalendar latex odt org))
 '(org-special-ctrl-a/e t)
 '(org-support-shift-select t)
 '(package-selected-packages
   '(consult counsel-spotify zzz-to-char flycheck-eglot treemacs-magit treemacs-nerd-icons eglot company-box flycheck-nim fira-code-mode ligature pdf-tools nerd-icons-dired nerd-icons-ivy-rich ess poly-R ess-R-data-view flycheck-plantuml plantuml-mode treemacs-projectile treemacs-all-the-icons treemacs-icons-dired bongo flycheck-google-cpplint google-c-style matlab-mode lsp-jedi key-assist fic-mode arduino-cli-mode company-arduino arduino-mode magit-todos nim-mode expand-region atom-dark-theme auto-package-update rust-mode move-dup counsel-tramp ssh-agency magit doom-modeline lsp-python-ms all-the-icons java-snippets yasnippet projectile flycheck lsp-python lsp-java dap-python dap-java dap-mode lsp-treemacs lsp-ivy lsp-ui lsp-mode company ivy-rich which-key use-package rainbow-delimiters popup javaimp counsel async))
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
 '(warning-suppress-types '((comp)))
 '(weechat-color-list
   (unspecified "#242728" "#323342" "#F70057" "#ff0066" "#86C30D" "#63de5d" "#BEB244" "#E6DB74" "#40CAE4" "#06d8ff" "#FF61FF" "#ff8eff" "#00b2ac" "#53f2dc" "#f8fbfc" "#ffffff")))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(font-lock-comment-face ((t (:foreground "#5C6370" :slant italic))))
 '(hi-black-b ((t (:background "dim gray" :foreground "black" :weight bold))))
 '(hi-blue ((t (:background "#51afef" :foreground "black"))))
 '(hi-green ((t (:background "#98be65" :foreground "black"))))
 '(hi-pink ((t (:background "#c678dd" :foreground "black"))))
 '(hi-yellow ((t (:background "#ECBE7B" :foreground "black")))))
(put 'upcase-region 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)
