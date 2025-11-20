;; ═════════════════════════════════════════════════════════════════════════════
;;  PERFORMANCE & STARTUP OPTIMIZATIONS
;; ═════════════════════════════════════════════════════════════════════════════

;; Reduce GC frequency during startup
(setq gc-cons-threshold (* 100 1024 1024)
      gc-cons-percentage 0.3)

;; macOS performance optimizations
(setq file-name-handler-alist nil)
(setq process-adaptive-read-buffering nil)

;; Display optimizations
(setq blink-cursor-mode nil)
(setq frame-inhibit-implied-resize t)
(setq font-lock-maximum-decoration 1)
(setq fast-but-imprecise-scrolling t)
(setq redisplay-dont-pause t)

;; Bidirectional text optimizations
(setq bidi-inhibit-bpa t)
(setq bidi-display-reordering nil)

;; Font cache optimization
(setq inhibit-compacting-font-caches t)

;; ═════════════════════════════════════════════════════════════════════════════
;;  UI CONFIGURATION
;; ═════════════════════════════════════════════════════════════════════════════

;; Disable UI elements
(tool-bar-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode -1)

;; Remove title bar / window decorations
(add-to-list 'initial-frame-alist '(undecorated . t))
(add-to-list 'default-frame-alist '(undecorated . t))

;; Startup screen
(setq inhibit-startup-screen t)
(setq inhibit-startup-message t)
(setq inhibit-startup-buffer-menu t)
(setq visible-bell nil)

;; Scrolling behavior
(setq scroll-conservatively 101
      scroll-margin 2
      scroll-step 1)

;; Font configuration
(add-to-list 'default-frame-alist `(font . "Iosevka Extended-20"))

;; Frame size
(add-to-list 'initial-frame-alist '(fullscreen . maximized))
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; ═════════════════════════════════════════════════════════════════════════════
;;  MACOS-SPECIFIC SETTINGS
;; ═════════════════════════════════════════════════════════════════════════════

(setq mac-option-key-is-meta t)
(setq mac-command-key-is-meta nil)

;; Clipboard integration for macOS terminal Emacs
(when (and (eq system-type 'darwin) (not (display-graphic-p)))
  (setq interprogram-cut-function
        (lambda (text &optional push)
          (let ((process-connection-type nil))
            (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
              (process-send-string proc text)
              (process-send-eof proc)))))
  (setq interprogram-paste-function
        (lambda ()
          (shell-command-to-string "pbpaste"))))

;; ═════════════════════════════════════════════════════════════════════════════
;;  EDITOR BEHAVIOR
;; ═════════════════════════════════════════════════════════════════════════════

;; Key bindings
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(global-unset-key (kbd "C-z"))

;; Editor settings
(electric-pair-mode t)
(setq tab-always-indent 'complete)
(setq require-final-newline t)
(setq create-lockfiles nil)

;; Auto-revert files when changed externally
(global-auto-revert-mode t)
(setq auto-revert-verbose nil)
(setq global-auto-revert-non-file-buffers t)

;; Fill column indicator
(setq-default display-fill-column-indicator-column 79)
(set-face-attribute 'fill-column-indicator nil :background 'unspecified :foreground "grey90")

;; Eshell
(setq eshell-destroy-buffer-when-process-dies t)

;; ═════════════════════════════════════════════════════════════════════════════
;;  FILE MANAGEMENT
;; ═════════════════════════════════════════════════════════════════════════════

;; Custom file (separate from init.el)
(setq custom-file (expand-file-name ".emacs.custom.el" user-emacs-directory))
(unless (file-exists-p custom-file)
  (with-temp-buffer (write-file custom-file)))
(load custom-file 'noerror 'nomessage)

;; Directory structure for user data
(let* ((base-dir user-emacs-directory)
       (tmp-dir  (expand-file-name "tmp/" base-dir))
       (var-dir  (expand-file-name "var/" base-dir))
       (socket-dir (expand-file-name "server/" base-dir)))

  ;; Ensure directories exist
  (dolist (dir (list tmp-dir var-dir))
    (make-directory dir t))

  ;; Backups and autosaves
  (setq backup-directory-alist `(("." . ,(expand-file-name "backups/" tmp-dir))))
  (setq auto-save-file-name-transforms `((".*" ,(expand-file-name "auto-saves/" tmp-dir) t)))
  (setq auto-save-list-file-prefix (expand-file-name "auto-saves/sessions/" tmp-dir))

  ;; Persistent data (non-temporary)
  (setq eshell-directory-name       (expand-file-name "eshell/" var-dir))
  (setq transient-history-file      (expand-file-name "transient/history.el" var-dir))
  (setq package-user-dir            (expand-file-name "elpa/" var-dir))

  ;; Server socket files
  ;; (setq server-socket-dir socket-dir)
  )

;; ═════════════════════════════════════════════════════════════════════════════
;;  PACKAGE MANAGEMENT
;; ═════════════════════════════════════════════════════════════════════════════

;; Package archives
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)
(add-to-list 'package-archives '("elpa" . "https://elpa.gnu.org/packages/") t)

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; use-package setup
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)
(setq use-package-always-defer t)

;; Note: if having compiling issue, run this:
;; (byte-recompile-directory package-user-dir nil 'force)

;; ═════════════════════════════════════════════════════════════════════════════
;;  COMPLETION FRAMEWORK
;; ═════════════════════════════════════════════════════════════════════════════

;; ─── Vertico ─────────────────────────────────────────────────────────────────
(use-package vertico
  :custom
  (vertico-resize t)
  (vertico-cycle t)
  :init
  (vertico-mode)
  (vertico-insert))

;; ─── Savehist ────────────────────────────────────────────────────────────────
;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :init
  (savehist-mode))

;; ─── Minibuffer Settings ─────────────────────────────────────────────────────
;; Allow nested minibuffers
;; (setq enable-recursive-minibuffers t)

;; Hide commands in M-x that don't work in current mode
(setq read-extended-command-predicate #'command-completion-default-include-p)

;; Make minibuffer prompt read-only and non-editable
(setq minibuffer-prompt-properties
      '(read-only t cursor-intangible t face minibuffer-prompt))

;; ─── Orderless ───────────────────────────────────────────────────────────────
(use-package orderless
  :custom
  (completion-styles '(partial-completion flex))
  (completion-category-overrides '((file (styles partial-completion))))
  (completion-category-defaults nil) ;; Disable defaults, use our settings
  (completion-pcm-leading-wildcard t)) ;; Emacs 31: partial-completion behaves like substring

;; ─── Marginalia ──────────────────────────────────────────────────────────────
(use-package marginalia
  :init
  (marginalia-mode))

;; ─── Consult ─────────────────────────────────────────────────────────────────
(use-package consult
  :bind (;; C-x bindings in `ctl-x-map'
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ;; M-g bindings in `goto-map'
         ("M-g e" . consult-compile-error)
         ("M-g r" . consult-grep-match)
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ;; M-s bindings in `search-map'
         ("M-s d" . consult-find)                  ;; Alternative: consult-fd
         ("M-s c" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
         ;; Minibuffer
         :map minibuffer-local-map
         ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history))                ;; orig. previous-matching-history-element

  :hook (completion-list-mode . consult-preview-at-point-mode)
  :init
  (advice-add #'register-preview :override #'consult-register-window)
  (setq register-preview-delay 0.5)

  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  :config
  (consult-customize
   consult-ripgrep consult-git-grep consult-grep consult-man
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   :preview-key '(:debounce 0.4 any)))

;; ═════════════════════════════════════════════════════════════════════════════
;;  VERSION CONTROL
;; ═════════════════════════════════════════════════════════════════════════════

(use-package magit)

;; Magit display settings
(setq magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
(add-hook 'magit-status-mode-hook #'delete-other-windows)

;; ═════════════════════════════════════════════════════════════════════════════
;;  EDITING ENHANCEMENTS
;; ═════════════════════════════════════════════════════════════════════════════

;; expand-region
(use-package expand-region
  :bind (("M-=" . er/expand-region)))

;; Company (completion)
(use-package company
  :init
  (global-company-mode))

;; ═════════════════════════════════════════════════════════════════════════════
;;  TREE-SITTER CONFIGURATION
;; ═════════════════════════════════════════════════════════════════════════════

(setq treesit-language-source-alist
      '((python      "https://github.com/tree-sitter/tree-sitter-python" "v0.23.6")
	(rust        "https://github.com/tree-sitter/tree-sitter-rust" "v0.23.3")
	(yaml        "https://github.com/tree-sitter-grammars/tree-sitter-yaml" "v0.7.2")
	(dockerfile  "https://github.com/camdencheek/tree-sitter-dockerfile")
	(html        "https://github.com/tree-sitter/tree-sitter-html")
        (css         "https://github.com/tree-sitter/tree-sitter-css" "v0.23.2")
        (javascript  "https://github.com/tree-sitter/tree-sitter-javascript" "v0.23.1")
        (typescript  "https://github.com/tree-sitter/tree-sitter-typescript" "v0.23.1" "typescript/src")))

;; ═════════════════════════════════════════════════════════════════════════════
;;  LSP / EGLOT CONFIGURATION
;; ═════════════════════════════════════════════════════════════════════════════

;; Flymake settings
(setq flymake-no-changes-timeout 0.2
      flymake-start-on-save-buffer t
      flymake-proc-allowed-file-name-masks nil) ;; don't analyze temp files

;; Disable excessive logging
(advice-add 'eglot--log-event :override #'ignore)

;; Eglot
(use-package eglot
  :hook ((eglot--managed-mode . breadcrumb-local-mode)
         (eglot--managed-mode . eglot-booster-mode))
  :bind (:map eglot-mode-map ("F2" . eglot-rename)
	      ("F6" . eglot-format-buffer))
  :config
  ;; Disable event logging for performance
  (customize-set-variable 'eglot-events-buffer-config '(:size 0))
  (setq eglot-events-buffer-size 0)
  (fset #'jsonrpc--log-event #'ignore)
  (setq jsonrpc-event-hook nil)

  ;; Eglot behavior
  (setq eglot-extend-to-xref t)
  (setq eglot-sync-connect 0
        eglot-autoreconnect nil
        eglot-send-changes-idle-time 0.1)

  ;; Eldoc settings
  (setq eldoc-echo-area-use-multiline-p t)
  (setq eldoc-idle-delay 0.2)

  ;; Disable certain server capabilities for performance
  (setq eglot-ignored-server-capabilities
        '(:codeActionProvider :documentHighlightProvider :semanticTokensProvider))

  ;; Language server configurations
  ;; Python: brew install pyright ruff basedpyright
  (add-to-list 'eglot-server-programs
               `(python-ts-mode . ,(eglot-alternatives '(("pyright-langserver" "--stdio")
                                                          ("basedpyright-langserver" "--stdio")
                                                          ("ruff" "server")))))

  ;; Rust: rustup component add rust-analyzer
  (add-to-list 'eglot-server-programs
               `(rust-ts-mode . ,(eglot-alternatives '(("rust-analyzer")))))

  ;; Workspace configuration
  (setq-default eglot-workspace-configuration
                '(:basedpyright
                  (:typeCheckingMode "recommended"
		   :analysis
		   (:diagnosticSeverityOverrides
		    (:reportUnusedCallResult "none")))
                  :inlayHints
                  (:callArgumentNames "all"
		   :functionReturnTypes t)
		  :pyright ()
		  :ruff ())))

;; ═════════════════════════════════════════════════════════════════════════════
;;  CODE FORMATTING
;; ═════════════════════════════════════════════════════════════════════════════

(use-package reformatter
  :config
  (reformatter-define ruff-check
    :program "ruff"
    :args `("check" "--fix" "--stdin-filename" ,buffer-file-name "-"))
  (reformatter-define ruff-organize-imports
    :program "ruff"
    :args `("check" "--select" "I" "--fix" "--stdin-filename" ,buffer-file-name "-"))
  (reformatter-define ruff-format
    :program "ruff"
    :args `("format" "--stdin-filename" ,buffer-file-name "-")))

;; ═════════════════════════════════════════════════════════════════════════════
;;  LANGUAGE-SPECIFIC MODES
;; ═════════════════════════════════════════════════════════════════════════════

;; ─── Python ──────────────────────────────────────────────────────────────────
(use-package python-ts-mode
  :ensure nil
  :hook ((python-ts-mode . display-fill-column-indicator-mode)
	 (python-ts-mode . eglot-ensure)
         (python-ts-mode . ruff-check-on-save-mode)
         (python-ts-mode . ruff-organize-imports-on-save-mode)
         (python-ts-mode . ruff-format-on-save-mode))
  :mode (("\\.py\\'" . python-ts-mode))
  :config
  (setq indent-tabs-mode nil)
  (setq tab-width 4)
  (setq electric-indent-inhibit nil)
  (electric-indent-mode 1))

;; ─── Rust ────────────────────────────────────────────────────────────────────
(use-package rust-ts-mode
  :ensure nil
  :hook ((rust-ts-mode . eglot-ensure))
  :mode (("\\.rs\\'" . rust-ts-mode)))

;; ─── TypeScript ──────────────────────────────────────────────────────────────
(use-package typescript-ts-mode
  :ensure nil
  :mode (("\\.ts\\'" . typescript-ts-mode)))

;; ─── JavaScript ──────────────────────────────────────────────────────────────
(use-package js-ts-mode
  :ensure nil
  :mode (("\\.js\\'" . js-ts-mode)))

;; ─── HTML ────────────────────────────────────────────────────────────────────
(use-package html-ts-mode
  :ensure nil
  :mode (("\\.html\\'" . html-ts-mode)))

;; ─── CSS ─────────────────────────────────────────────────────────────────────
(use-package css-ts-mode
  :ensure nil
  :mode (("\\.css\\'" . css-ts-mode)))

;; ─── YAML ────────────────────────────────────────────────────────────────────
(use-package yaml-ts-mode
  :ensure nil
  :mode (("\\.yaml\\'" . yaml-ts-mode)
	 ("\\.yml\\'" . yaml-ts-mode)))

;; ─── Dockerfile ──────────────────────────────────────────────────────────────
(use-package dockerfile-ts-mode
  :ensure nil
  :mode (("Dockerfile\\'" . dockerfile-ts-mode)
         ("\\.[Dd]ockerfile\\'" . dockerfile-ts-mode)))

;; ═════════════════════════════════════════════════════════════════════════════
;;  CUSTOM LIBRARIES
;; ═════════════════════════════════════════════════════════════════════════════

(add-to-list 'load-path (expand-file-name "lisp/" user-emacs-directory))
(require 'eglot-booster)
(require 'vscode)
(require 'breadcrumb)

;; Enable vscode-mode globally
(global-vscode-mode 1)

;; ═════════════════════════════════════════════════════════════════════════════
;;  THEME
;; ═════════════════════════════════════════════════════════════════════════════

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(load-theme 'github-dark-colorblind t)

;; ═════════════════════════════════════════════════════════════════════════════
;;  MODE LINE
;; ═════════════════════════════════════════════════════════════════════════════

(setq mode-line-right-align-edge 'right-fringe)
(setq-default
 mode-line-format
 '("%e"
   mode-line-front-space
   (:propertize
    ("" mode-line-mule-info mode-line-client mode-line-modified
     mode-line-remote mode-line-window-dedicated)
    display (min-width (6.0)))
   mode-line-frame-identification mode-line-buffer-identification " "
   (project-mode-line project-mode-line-format)
   ;; Flymake
   (:eval (when (bound-and-true-p flymake-mode)
            '(" " flymake-mode-line-counters)))
   ;; Align right
   mode-line-format-right-align
   "Ln: %l "
   mode-line-end-spaces))

;; ═════════════════════════════════════════════════════════════════════════════
;;  ORG MODE CONFIGURATION
;; ═════════════════════════════════════════════════════════════════════════════

;; Org agenda files
(setq org-agenda-files
      '("~/org/todo.org"
        "~/org/work.org"
        "~/org/home.org"
        "~/org/projects.org"
        "~/org/inbox.org"))

;; Org behavior
(setq org-agenda-start-with-log-mode t)
(setq org-log-done 'time)
(setq org-log-into-drawer t)
(setq org-treat-insert-todo-heading-as-state-change t)
(setq org-lowest-priority ?E)

;; TODO keywords
(setq org-todo-keywords
      '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!)")
	(sequence "BACKLOG(b)" "PLAN(p)" "READY(r)" "ACTIVE(a)" "REVIEW(v)" "WAIT(w@/!)" "HOLD(h)" "|" "COMPLETED(c)" "CANC(k@)")))

;; Tags
(setq org-tag-alist
      '((:startgroup)
	; Put mutually exclusive tags here
	(:endgroup)
	("@errand" . ?E)
	("@home" . ?H)
	("@work" . ?W)
	("agenda" . ?a)
	("planning" . ?p)
	("publish" . ?P)
	("note" . ?n)
	("idea" . ?i)))

;; Custom agenda commands
(setq org-agenda-custom-commands
      '(("d" "Dashboard"
	 ((agenda "" ((org-deadline-warning-days 7)))
	  (todo "NEXT"
		((org-agenda-overriding-header "Next Tasks")))
	  (tags-todo "agenda/ACTIVE" ((org-agenda-overriding-header "Active Projects")))))))

;; Org bullets
(use-package org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
