;; Sane defaults
(setq package-enable-at-startup nil)
(setq inhibit-startup-screen t)
(setq initial-scratch-message nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(global-display-line-numbers-mode)
(setq-default tab-width 4)
(setq make-backup-files nil)
(setq auto-save-default nil)

;; Disable default echo-area-message and ensure last message is blank
(put 'inhibit-startup-echo-area-message 'saved-value t)
(setq inhibit-startup-echo-area-message (user-login-name))
(defun display-startup-echo-area-message ()
  (message ""))

;; Default frame size, override in early-init.el
(defcustom edmacs/frame-width 150
  "Initial frame width, override in early-init.el"
  :type 'integer)

(defcustom edmacs/frame-height 60
  "Initial frame height, override in early-init.el"
  :type 'integer)

(defcustom edmacs/frame-font "JetBrains Mono 12"
  "Default Frame Font, override in early-init.el")

(defun edmacs/init-new-frame (frame)
  (set-frame-size frame edmacs/frame-width edmacs/frame-height)
  (set-frame-font edmacs/frame-font nil t)
  (menu-bar-mode -1)
  (tool-bar-mode -1))

;; Setup frame for initially loaded frame
(edmacs/init-new-frame (selected-frame))

;; Ensure that frames are correctly sized for new frames/emacsclient as well
(add-hook 'after-make-frame-functions 'edmacs/init-new-frame)

;; Zoom on C-mousewheel
(global-set-key [C-mouse-4] 'text-scale-increase)
(global-set-key [C-mouse-5] 'text-scale-decrease)

;; LSP Optimizations
(setq read-process-output-max 64000)
(setq gc-cons-threshold 100000000)
;; LSP Must be recompiled after asking it to use plists instead of hash-table for deserializing objects
(setenv "LSP_USE_PLISTS" "1")

;; Prevent dired from opening new buffers when visiting
(setq dired-kill-when-opening-new-dired-buffer t)

;; Electric pair mode everywere but in minibuffer
(electric-pair-mode)
(defun inhibit-electric-pair-mode (char)
  (minibufferp))
(setq electric-pair-inhibit-predicate #'inhibit-electric-pair-mode)

;; Configure Straight package manager
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(use-package use-package)
;; Configure use-package to use straight.el by default
(use-package straight
  :custom
  (straight-use-package-by-default t))

;; Icons
(use-package nerd-icons
  :custom
  (nerd-icons-font-family "JetBrainsMono Nerd Font"))

(use-package nerd-icons-dired)
(use-package nerd-icons-completion)
(use-package nerd-icons-ibuffer)
(add-hook 'dired-mode-hook #'nerd-icons-dired-mode)

;; Window management
(use-package ace-window
  :init
  (global-set-key (kbd "M-o") 'ace-window))

;; Theme
(use-package doom-themes
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-palenight t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(use-package doom-modeline
  :config
  (set-face-background 'doom-modeline-bar (face-background 'mode-line))
  (setq doom-modeline-bar-width 1)
  :init (doom-modeline-mode 1))

;; Project management
(use-package projectile
  :config
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (projectile-mode +1))

;; Which key
(use-package which-key
  :config
  (setq which-key-idle-delay 0.1)
  (which-key-mode))

;; Ripgrep, also supports projectile C-l p s r
(use-package rg)

;; Magit
(use-package magit)

;; YAML
(use-package yaml-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode)))

;; Treemacs
(use-package treemacs
  :init
  (global-set-key (kbd "M-0") 'treemacs-select-window))

;; LSP
(add-to-list 'load-path (expand-file-name "lib/lsp-mode" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "lib/lsp-mode/clients" user-emacs-directory))

(use-package go-mode)
(add-hook 'go-mode-hook
          (lambda ()
            (add-hook 'before-save-hook
                      #'gofmt-before-save
                      nil t)))

(add-to-list 'auto-mode-alist '("\\.go\\'" . go-mode))
(use-package go-tag)

(use-package lsp-java :config (add-hook 'java-mode-hook 'lsp))
(use-package kotlin-mode)

(use-package lsp-mode
  :hook (
	 (svelte-mode . lsp-deferred)
	 (typescript-mode . lsp-deferred)
	 (javascript-mode . lsp-deferred)
	 (go-mode . lsp-deferred)
	 (kotlin-mode . lsp-deferred)
     (lsp-mode . lsp-enable-which-key-integration))
  :init
  (setq lsp-file-watch-threshold 3000)
  :config
  (define-key lsp-mode-map (kbd "C-c l") lsp-command-map)
  :commands (lsp lsp-deferred))

(use-package lsp-ui :commands lsp-ui-mode)

;;Sveltee
(use-package svelte-mode)

;; TypeScript
(use-package typescript-mode)

;; TailwindCSS
(use-package lsp-tailwindcss
  :init
  (setq lsp-tailwindcss-add-on-mode t)
  :config
  (setq lsp-tailwindcss-major-modes '(svelte-mode html-mode web-mode css-mode typescript-mode)))

;; Java LSP Server must use recent version of Java, but default projects to Java 8
(setenv "JAVA_HOME" "/usr/lib/jvm/java-17-jdk/")
;; (setq lsp-java-import-maven-enabled t)
(setq lsp-java-import-maven-offline-enabled t)
(setq lsp-java-maven-download-sources nil)
(setq lsp-java-java-path "/usr/lib/jvm/java-17-jdk/bin/java")
(setq lsp-java-configuration-runtimes '[(:name "JavaSE-1.8"
											   :path "/usr/lib/jvm/java-8-jdk"
											   :default t)])
;; Consider reducing debug output from LSP
;; (setq lsp-inhibit-message t)

;; SQL 
(add-hook 'sql-mode-hook 'lsp)
(setq lsp-sqls-workspace-config-path nil)

;; SQL Source Code Blocks in Org mode using sql-mode
(use-package ob-sql-mode)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((sql . t)))

;; Do not prompt for executing Org mode SQL
(setq org-confirm-babel-evaluate
      (lambda (lang body)
        (not (string= lang "sql-mode"))))

;; Debugging
(use-package dap-mode
  :after lsp-mode
  :config (dap-auto-configure-mode)
  :init (require 'dap-dlv-go))

;; Snippet engine used by LSP for parameter completion
(use-package yasnippet
  :config
  (yas-global-mode))

;; Snippets used by yasnippet for almost every language
(use-package yasnippet-snippets)

;; Completion
(use-package company
  :config
  (setq company-idle-delay 0.1)
  (setq company-minimum-prefix-length 1))

(add-hook 'after-init-hook 'global-company-mode)

;; Hydra is used by many extensions to provide intermittent menus
(use-package hydra)

;; Syntax checking
(use-package flycheck
  :init
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))
  (global-flycheck-mode))

;; Syntax hl
(use-package tree-sitter)
(use-package tree-sitter-langs)
(tree-sitter-require 'go)

;; Markdown
(use-package markdown-mode
  :mode ("README\\.md'" . gfm-mode)
  :init (setq markdown-command "multimarkdown")
  :bind (:map markdown-mode-map
			  ("C-c C-e" . markdown-do)))

(add-to-list 'auto-mode-alist '("\\.go\\'" . go-mode))

;; Enable vertico
(use-package vertico
  :init (vertico-mode))

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :init
  (savehist-mode))

;; Further vertico config
(use-package emacs
  :init
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Enable recursive minibuffers
  (setq enable-recursive-minibuffers t))

;; Allow searches in vertico from anywhere in strings
(use-package orderless
  :init
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

;; Add more information to menus controlled by Vertico
(use-package marginalia
  :after vertico
  :custom
  (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
  :init
  (marginalia-mode))

;; HTTP REST Client
(use-package restclient
  :mode (("\\\.http\\'" . restclient-mode)))
(use-package company-restclient)
(add-to-list 'company-backends 'company-restclient)

;; Load user settings if exists
(let ((edmacs/custom-init-file (concat user-emacs-directory "/user.el")))
  (when (file-exists-p edmacs/custom-init-file)
	(load-file edmacs/custom-init-file)))

(provide 'init)
;;; init.el ends here
