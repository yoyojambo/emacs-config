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
      '((java-mode . "java")))
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
  :custom
  (doom-themes-enable-bold t)
  (doom-themes-enable-italic t)
  :config
  (load-theme 'doom-ayu-dark t)
  )


(doom-themes-org-config)
(doom-themes-visual-bell-config)

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")

;; Taken from -> https://tsdh.org/posts/2021-06-06-update-all-emacs-packages-from-the-command-line.html
(use-package auto-package-update
  :ensure t
  :config
  ;; Yes, please delete the old version on updates.
  (setq auto-package-update-delete-old-versions t))

;; Font
(set-face-attribute 'default nil :font "Cascadia Code NF" :height 110)

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

(global-ligature-mode 1)

;; Tree-sitter
(require 'treesit)
(setq treesit-language-source-alist
   '((bash "https://github.com/tree-sitter/tree-sitter-bash")
     (cmake "https://github.com/uyha/tree-sitter-cmake")
     (css "https://github.com/tree-sitter/tree-sitter-css")
     (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
     (elisp "https://github.com/Wilfred/tree-sitter-elisp")
     (go "https://github.com/tree-sitter/tree-sitter-go" "v0.25.0") ;; go treesitter changed from method_spec to method_elem, and that broke templ's treesitter... keep in mind
	 (templ "https://github.com/vrischmann/tree-sitter-templ")
     (html "https://github.com/tree-sitter/tree-sitter-html")
     (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
     (json "https://github.com/tree-sitter/tree-sitter-json")
     (make "https://github.com/alemuller/tree-sitter-make")
     (markdown "https://github.com/ikatyang/tree-sitter-markdown")
     (python "https://github.com/tree-sitter/tree-sitter-python")
     (toml "https://github.com/tree-sitter/tree-sitter-toml")
     (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
     (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
     (yaml "https://github.com/ikatyang/tree-sitter-yaml")
	 (astro "https://github.com/virchau13/tree-sitter-astro")
	 ))

(use-package astro-ts-mode
  :mode "\\.astro\\'")

(use-package google-c-style
  :hook (c++-mode . google-set-c-style))

;; Package for usint M-up and M-down to move lines around
(use-package move-dup)

;; Nim config
(use-package nim-mode
  :magic ("%NIM" . nim-mode))

;; Go config
(use-package go-ts-mode
  :mode "\\.go\\'")

(add-hook 'go-mode-hook
	  (lambda ()
	    (setq tab-width 4)))

;; OCaml config
(use-package tuareg
  :ensure t
  :mode (("\\.ocamlinit\\'" . tuareg-mode)))

(use-package ocaml-eglot
  :ensure t
  :after tuareg
  :hook
  (tuareg-mode . ocaml-eglot)
  (tuareg-mode . (lambda () (local-unset-key (kbd "C-h ."))))
  (ocaml-eglot-mode . eglot-ensure)
  (ocaml-eglot-mode . (lambda ()
						(add-hook #'before-save-hook #'eglot-format nil t)))
  :config
  (setq ocaml-eglot-syntax-checker 'flycheck))

;; I don't want to keep writing pipes
(defun my-tuareg-insert-match-pipe ()
  "Insert a newline and a pattern matching pipe character."
  (interactive)
  (end-of-line)
  (newline)
  (insert "| ")
  (indent-according-to-mode))

(add-hook 'tuareg-mode-hook
          (lambda ()
            (local-set-key (kbd "M-RET") 'my-tuareg-insert-match-pipe)))

;; Plantuml-mode
(use-package plantuml-mode
  :mode "\\.puml\\'"
  :config (setq plantuml-default-exec-mode 'jar))

;; Org mode
(add-hook 'org-mode-hook
	  (lambda ()
	    (org-indent-mode)
	    (setq org-html-validation-link nil)
	    (setq org-export-with-section-numbers nil)
	    (setq org-export-with-timestamps nil)
	    (setq org-html-preamble nil)
	    (setq org-image-actual-width nil)
		(setq org-todo-keywords
			  '((sequence "TODO(t)" "PROG(p!/!)" "|" "DONE(d!)" "CANCELED(c@)")))
		(run-with-timer nil 1
						(lambda ()
						  (when (and (derived-mode-p 'org-mode)
									 org-inline-image-overlays)
							(org-redisplay-inline-images))))
		))




;; thanks, System Crafters
;; https://systemcrafters.net/org-mode-productivity/custom-org-agenda-views
(global-set-key (kbd "C-c o a") #'org-agenda)

;; thanks, ChatGpt
(defun yoyojambo/org-latex-needspace-setup ()
  "Configure Org LaTeX export to insert \\needspace before headings.
Ensures the needspace package is loaded and modifies headline export."
  (interactive)
  ;; Add the package to the export preamble
  (add-to-list 'org-latex-packages-alist '("" "needspace"))
  ;; Override headline formatting
  (setq-local org-latex-format-headline-function
              (lambda (todo todo-type priority text tags info)
                (concat "\\needspace{5\\baselineskip}\n"
                        (org-latex-format-headline-default-function
                         todo todo-type priority text tags info))))
  (message "Org LaTeX needspace setup enabled for this buffer."))

(with-eval-after-load 'ox-latex
  (add-to-list 'org-latex-classes
               '("tec-assignment"
                 "\\documentclass[12pt]{article}
\\usepackage{graphicx}
\\usepackage[utf8]{inputenc}

\\makeatletter
% Define internal variables with defaults
\\def\\@coursename{}
\\def\\@groupnum{}
\\def\\@assignmentdate{}
\\def\\@logofile{tec-logo.png} % Default filename

% Create setter commands for the org file
\\newcommand{\\coursename}[1]{\\def\\@coursename{#1}}
\\newcommand{\\groupnum}[1]{\\def\\@groupnum{#1}}
\\newcommand{\\logofile}[1]{\\def\\@logofile{#1}}
\\newcommand{\\assignmentdate}[1]{\\def\\@assignmentdate{#1}}

\\renewcommand{\\maketitle}{
  \\begin{titlepage}
      \\centering
      
      
      % Uses the variable for the image
      \\includegraphics[width=0.6\\textwidth]{\\@logofile}
      
      \\vspace{1.5cm}
      
      {\\Large Instituto Tecnológico y de Estudios Superiores de Monterrey \\par}
      \\vspace{2cm}
      
      \\ifx\\@coursename\\@empty\\else
        {\\Large \\textit{\\@coursename} \\par}
        \\vspace{0.5cm}
      \\fi
      
      \\ifx\\@groupnum\\@empty\\else
        {\\large \\@groupnum \\par}
        \\vspace{2cm}
      \\fi
      
      {\\Large \\textbf{\\@title} \\par}
      \\vspace{1.5cm}
      
      {\\large \\textbf{\\@author} \\par}

      \\ifx\\@assignmentdate\\@empty\\else
        \\vspace{1.5cm}
        {\\large \\@assignmentdate \\par}
      \\fi
      \\vfill
  \\end{titlepage}
}
\\makeatother"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")))
  (add-to-list 'org-latex-classes
               '("ieee"
                 "\\documentclass[conference]{IEEEtran}"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")))
  
  (add-to-list 'org-latex-classes
               '("lncs"
                 "\\documentclass{llncs}"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))))

;; https://github.com/jwiegley/use-package#magic-handlers
(use-package pdf-tools
  :load-path "site-lisp/pdf-tools/lisp"
  :magic ("%PDF" . pdf-view-mode)
  :config
  (setq revert-without-query '(".pdf")) ; So it does not ask on each org export
  (pdf-tools-install :no-query)
  (setq-default pdf-view-display-size 'fit-height)
  (define-key pdf-view-mode-map (kbd "C-s") 'isearch-forward))

; Not convinced the pros outweigh the cons
;; (use-package org-modern
;;   :hook (org-mode . org-modern-mode))

(use-package vterm
    :ensure t)

;; Disable line numbers for following modes
(dolist
    (mode-hook
     '(org-mode-hook
       term-mode-hook
       vterm-mode-hook
       eshell-mode-hook
       shell-mode-hook
       dired-mode-hook
       image-mode-hook
       pdf-view-mode-hook
       help-mode-hook))
  (add-hook mode-hook (lambda () (display-line-numbers-mode 0))))

;; ;; Eshell prompt colors
;; (defun my-eshell-prompt ()
;;   "Highlight eshell pwd and prompt separately."
;;   (mapconcat
;;    (lambda (list)
;;      (propertize (car list)
;;                  'read-only      t
;;                  'font-lock-face (cdr list)
;;                  'front-sticky   '(font-lock-face read-only)
;;                  'rear-nonsticky '(font-lock-face read-only)))
;;    `((,(abbreviate-file-name (eshell/pwd)) :foreground "dodger blue")
;;      (,(if (zerop (user-uid)) " # " " $ ") :foreground "orchid"))
;;    ""))

;; (setq eshell-highlight-prompt nil
;;       eshell-prompt-function  #'my-eshell-prompt)

;; vterm instead of shell for project-compile
(defun my-project-shell ()
  "Start an inferior shell in the current project's root directory.
If a buffer already exists for running a shell in the project's root,
switch to it.  Otherwise, create a new shell buffer.
With \\[universal-argument] prefix arg, create a new inferior shell buffer even
if one already exists."
  (interactive)
  (require 'comint)
  (let* ((default-directory (project-root (project-current t)))
         (default-project-shell-name (project-prefixed-buffer-name "shell"))
         (shell-buffer (get-buffer default-project-shell-name)))
    (if (and shell-buffer (not current-prefix-arg))
        (if (comint-check-proc shell-buffer)
            (pop-to-buffer shell-buffer (bound-and-true-p display-comint-buffer-action))
          (vterm shell-buffer))
      (vterm (generate-new-buffer-name default-project-shell-name)))))

(advice-add 'project-shell :override #'my-project-shell)

;; Expand region (C-+)
(use-package expand-region)

;; Rainbow Delimiters
(use-package rainbow-delimiters
  :ensure t
  :init
  (progn
    (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)))

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
  :init
  (ivy-mode 1)
  :custom
  (ivy-use-virtual-buffers t)
  (ivy-count-format "(%d/%d) " "Sets the format around the option counter in ivy, counsel, swiper.")  
  )

(use-package ivy-rich
  :init
  (ivy-rich-mode 1))

(use-package counsel-tramp)

;; nerd-icons
(use-package nerd-icons)

(use-package nerd-icons-dired
  :hook
  (dired-mode . nerd-icons-dired-mode))

(use-package nerd-icons-ivy-rich
  :ensure t
  :init
  (nerd-icons-ivy-rich-mode 1))

(use-package ivy-posframe
  :ensure t
  :config (ivy-posframe-mode 1)
  :custom (ivy-posframe-height-alist '((swiper . 20)
									   (t      . 40)))
  )

;; LSP-Mode
;; (use-package lsp-mode
;;   :hook (lsp-mode . yas-minor-mode-on)
;;   :defer (use-package lsp-ui))

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


;; web-mode
(use-package web-mode
  :defer t
  :ensure t)

;; Eglot
(use-package eglot
  :ensure t
  :config
  ;; Register the server for both regular and tree-sitter modes
  (add-to-list 'eglot-server-programs
               '((astro-mode astro-ts-mode) . ("astro-ls" "--stdio"
                                               :initializationOptions
                                               (:typescript (:tsdk "./node_modules/typescript/lib")))))
  (add-to-list 'eglot-server-programs
               '(nim-mode . ("nimlsp")))
  )

; Faster IO with a wrapper around the interaction with LSPs
(use-package eglot-booster
  :after eglot
  :config (eglot-booster-mode))

(use-package projectile)
(use-package flycheck :ensure)
(use-package flycheck-eglot
 :ensure t
 :after (flycheck eglot)
 :config
  (global-flycheck-eglot-mode 1))
(use-package flycheck-nim)
(add-hook 'eglot-managed-mode-hook #'flycheck-eglot-mode)

;; Fix weird think about flycheck and eglot and hovering over variables
(add-hook 'eglot-managed-mode-hook
          (lambda ()
            ;; 1. Remove the eglot functions from wherever they are to avoid duplicates
            (setq-local eldoc-documentation-functions
                        (delete 'eglot-hover-eldoc-function 
                                (delete 'eglot-signature-eldoc-function 
                                        (delete t eldoc-documentation-functions))))
            ;; 2. Push them to the front in the preferred order
            (push 'eglot-signature-eldoc-function eldoc-documentation-functions)
            (push 'eglot-hover-eldoc-function eldoc-documentation-functions)
            ;; 3. Add 't' at the very end as a fallback
            (add-to-list 'eldoc-documentation-functions t t)))

(use-package copilot
  :ensure t
  :bind (:map copilot-completion-map
              ("TAB" . copilot-accept-completion)
              ("<tab>" . copilot-accept-completion))
  :config
  ;; custom function to integrate copilot with normal tab behavior
  (defun my/copilot-tab ()
    (interactive)
    (or (copilot-accept-completion)
        (indent-for-tab-command)))

  ;; globally bind tab in prog-mode buffers
  (define-key copilot-mode-map (kbd "C-TAB") #'my/copilot-tab)
  (define-key copilot-mode-map (kbd "C-<tab>") #'my/copilot-tab))

;; (use-package yasnippet
;;   :ensure t
;;   :config
;;   (use-package yasnippet-snippets
;;     :ensure t)
;;   ;(setq company-backends '((:separate company-yasnippet company-capf))
;;   (global-set-key (kbd "<M-return>") 'yas-expand)
;;   (yas-reload-all)
;;   )

(use-package yasnippet
  :hook (prog-mode . yas-minor-mode)
  :config
  ;(global-set-key (kbd "<M-return>") 'yas-expand) ;; Made optional so org mode works
  ;(yas-reload-all)
  )

(yas-global-mode 1)

(defun mine/activate-yas-expand ()
  "Sets locally the M-return key to 'yas-expand'"
  (interactive)
  (local-set-key (kbd "<M-return>") 'yas-expand))

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
 '(calendar-week-start-day 1)
 '(company-box-frame-behavior 'point)
 '(company-quickhelp-color-background "#3E4452")
 '(company-quickhelp-color-foreground "#ABB2BF")
 '(compilation-message-face 'default)
 '(connection-local-criteria-alist
   '(((:application eshell) eshell-connection-default-profile)
	 ((:application tramp) tramp-connection-local-default-system-profile
	  tramp-connection-local-default-shell-profile)))
 '(connection-local-profile-alist
   '((eshell-connection-default-profile (eshell-path-env-list))
	 (tramp-connection-local-darwin-ps-profile
	  (tramp-process-attributes-ps-args "-acxww" "-o"
										"pid,uid,user,gid,comm=abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
										"-o" "state=abcde" "-o"
										"ppid,pgid,sess,tty,tpgid,minflt,majflt,time,pri,nice,vsz,rss,etime,pcpu,pmem,args")
	  (tramp-process-attributes-ps-format (pid . number) (euid . number) (user . string)
										  (egid . number) (comm . 52) (state . 5)
										  (ppid . number) (pgrp . number) (sess . number)
										  (ttname . string) (tpgid . number)
										  (minflt . number) (majflt . number)
										  (time . tramp-ps-time) (pri . number)
										  (nice . number) (vsize . number) (rss . number)
										  (etime . tramp-ps-time) (pcpu . number)
										  (pmem . number) (args)))
	 (tramp-connection-local-busybox-ps-profile
	  (tramp-process-attributes-ps-args "-o"
										"pid,user,group,comm=abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
										"-o" "stat=abcde" "-o"
										"ppid,pgid,tty,time,nice,etime,args")
	  (tramp-process-attributes-ps-format (pid . number) (user . string) (group . string)
										  (comm . 52) (state . 5) (ppid . number)
										  (pgrp . number) (ttname . string)
										  (time . tramp-ps-time) (nice . number)
										  (etime . tramp-ps-time) (args)))
	 (tramp-connection-local-bsd-ps-profile
	  (tramp-process-attributes-ps-args "-acxww" "-o"
										"pid,euid,user,egid,egroup,comm=abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
										"-o"
										"state,ppid,pgid,sid,tty,tpgid,minflt,majflt,time,pri,nice,vsz,rss,etimes,pcpu,pmem,args")
	  (tramp-process-attributes-ps-format (pid . number) (euid . number) (user . string)
										  (egid . number) (group . string) (comm . 52)
										  (state . string) (ppid . number) (pgrp . number)
										  (sess . number) (ttname . string)
										  (tpgid . number) (minflt . number)
										  (majflt . number) (time . tramp-ps-time)
										  (pri . number) (nice . number) (vsize . number)
										  (rss . number) (etime . number) (pcpu . number)
										  (pmem . number) (args)))
	 (tramp-connection-local-default-shell-profile (shell-file-name . "/bin/sh")
												   (shell-command-switch . "-c"))
	 (tramp-connection-local-default-system-profile (path-separator . ":")
													(null-device . "/dev/null"))))
 '(create-lockfiles nil)
 '(custom-safe-themes
   '("9b9d7a851a8e26f294e778e02c8df25c8a3b15170e6f9fd6965ac5f2544ef2a9"
	 "83550d0386203f010fa42ad1af064a766cfec06fc2f42eb4f2d89ab646f3ac01"
	 "eca44f32ae038d7a50ce9c00693b8986f4ab625d5f2b4485e20f22c47f2634ae"
	 "02f57ef0a20b7f61adce51445b68b2a7e832648ce2e7efb19d217b6454c1b644"
	 "5b7c31eb904d50c470ce264318f41b3bbc85545e4359e6b7d48ee88a892b1915"
	 "3e5c1261d06395a74566da3af413ab909a2049e0c52d9297a5e2d6823bf189d6"
	 "44546a3c5032ace263613f39669a66c604523001135f5b42ea583c9abf6f0a5e"
	 "493434ed95de30b7648c293fc482c0bb3e3ba95543bdd936d89490da0ef5ebd5"
	 "6c12baea488aa868d32ee648fd5a704a351dd4d8689da6e21459200d386eaab1"
	 "7affd7ecac23d6f5f43f115d2ce3f9a95f1d65c2da6cedddeae53ecf1b7470f3"
	 "3bea339b9d83c48cddd1080494bc4971b7a85a78c51545fcf1429e7838f8b918"
	 "aaceba7dd433b4eed1de887b5c72a53f014237042704a441066e933235a5ab3a"
	 "12db058ce4ba460e067e331a67dbb05c4406d8c0d5e4504cebc059cffae55672"
	 "af9e9a92b17bb6f50d623867e1da6db47e015c30091f325dbe473abe7b397ba4"
	 "a31ca6382a13b79c63f7cfbf535099b73c0496837dc255b7158d3836488739db"
	 "3db307fb06cedec4f2f6dfcbc189ffc26bca9653d7e149643d451b8411a8f039"
	 "0c860c4fe9df8cff6484c54d2ae263f19d935e4ff57019999edbda9c7eda50b8"
	 "ebbd4bbb0f017cb09f7a3b1363b83dfde0c5f4970cda2705419457366cd2de91" default))
 '(diff-font-lock-prettify t)
 '(doc-view-continuous t)
 '(doom-ayu-dark-brighter-comments t)
 '(doom-ayu-dark-brighter-modeline t)
 '(doom-ayu-dark-comment-bg t)
 '(doom-modeline-mode t)
 '(doom-modeline-project-name t)
 '(doom-modeline-time-icon nil)
 '(doom-themes-enable-bold t)
 '(doom-themes-enable-italic t)
 '(electric-pair-mode t)
 '(fill-column 90)
 '(go-ts-mode-indent-offset 4)
 '(highlight-changes-colors '("#ff8eff" "#ab7eff"))
 '(hl-sexp-background-color "#1c1f26")
 '(json-ts-mode-indent-offset 4)
 '(lsp-clangd-binary-path "/usr/bin/clang")
 '(lsp-clangd-version "14.0.0")
 '(lsp-clients-clangd-executable "/usr/bin/clangd")
 '(magit-diff-use-overlays nil)
 '(magit-log-auto-more t)
 '(org-agenda-files '("~/agenda.org"))
 '(org-agenda-loop-over-headlines-in-active-region nil)
 '(org-agenda-todo-ignore-deadlines nil)
 '(org-babel-load-languages
   '((emacs-lisp . t) (python . t) (sql . t) (plantuml . t) (R . t) (shell . t)))
 '(org-confirm-babel-evaluate nil)
 '(org-export-backends '(ascii beamer html icalendar latex md odt org))
 '(org-export-use-babel nil)
 '(org-export-with-section-numbers nil)
 '(org-export-with-timestamps nil)
 '(org-image-actual-width nil)
 '(org-latex-image-default-width ".9\\linewidth")
 '(org-latex-src-block-backend 'listings)
 '(org-list-allow-alphabetical t)
 '(org-plantuml-jar-path "~/plantuml.jar")
 '(org-special-ctrl-a/e t)
 '(org-support-shift-select t)
 '(package-selected-packages
   '(astro-ts-mode auto-package-update chatgpt-shell colorful-mode company-box
				   company-quickhelp copilot counsel-tramp dockerfile-mode doom-modeline
				   doom-themes eglot-booster ess expand-region flycheck-eglot flycheck-nim
				   go-mode google-c-style ivy-posframe json-rpc just-mode just-ts-mode
				   leetcode ligature lsp-ui magit merlin move-dup nerd-icons-dired
				   nerd-icons-ivy-rich nim-mode ob-nim ocaml-eglot ocp-indent org-modern
				   org-web-tools ox-pandoc pandoc-mode pdf-tools plantuml-mode projectile
				   rainbow-delimiters ssh-agency templ-ts-mode tuareg vterm web-mode
				   which-key yasnippet-snippets))
 '(package-vc-selected-packages
   '((eglot-booster :vc-backend Git :url "https://github.com/jdtsmith/eglot-booster")))
 '(pos-tip-background-color "#E6DB74")
 '(pos-tip-foreground-color "#242728")
 '(sql-mysql-login-params '(user password server database port))
 '(tab-width 4)
 '(tetris-x-colors
   [[229 192 123] [97 175 239] [209 154 102] [224 108 117] [152 195 121] [198 120 221]
	[86 182 194]])
 '(treesit-font-lock-level 4)
 '(vterm-clear-scrollback-when-clearing t)
 '(warning-display-at-bottom nil)
 '(warning-minimum-level :error)
 '(warning-suppress-types '((ssh-agency ssh-add) (comp)))
 '(weechat-color-list
   (unspecified "#242728" "#323342" "#F70057" "#ff0066" "#86C30D" "#63de5d" "#BEB244"
				"#E6DB74" "#40CAE4" "#06d8ff" "#FF61FF" "#ff8eff" "#00b2ac" "#53f2dc"
				"#f8fbfc" "#ffffff")))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(hi-black-b ((t (:background "dim gray" :foreground "black" :weight bold)))))
(put 'upcase-region 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)
(put 'downcase-region 'disabled nil)
