;; -*- lexical-binding: t; -*-

(setenv "MACOSX_DEPLOYMENT_TARGET" "27.0")

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
(setq blink-cursor-mode t)
(setq frame-inhibit-implied-resize t)
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
(setq tool-bar-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode 0)

;; Remove title bar / window decorations
(add-to-list 'initial-frame-alist '(undecorated . t))
(add-to-list 'default-frame-alist '(undecorated . t))

;; Startup screen
(setq inhibit-startup-screen t)
(setq inhibit-startup-message t)
(setq inhibit-startup-buffer-menu t)
(setq visible-bell t)

;; Scrolling behavior
(setq scroll-conservatively 101
      scroll-margin 2
      scroll-step 1)

;; Font configuration
(add-to-list 'default-frame-alist `(font . "Terminus (TTF)-20"))

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
(pending-delete-mode t)
(global-hl-line-mode 1)
(setq isearch-lazy-count t)

;; Auto-revert files when changed externally
(global-auto-revert-mode t)
(setq auto-revert-verbose nil)
(setq global-auto-revert-non-file-buffers t)

;; Fill column indicator
(setq-default display-fill-column-indicator-column 79)
(set-face-attribute 'fill-column-indicator nil :background 'unspecified :foreground "grey90")

;; Shell
(setq shell-kill-buffer-on-exit t)

;; Eshell
(setq eshell-destroy-buffer-when-process-dies t)

;; Dired
(setq dired-dwim-target t)

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
   consult-source-bookmark consult-source-file-register
   consult-source-recent-file consult-source-project-recent-file
   :preview-key '(:debounce 0.4 any)))

;; ═════════════════════════════════════════════════════════════════════════════
;;  VERSION CONTROL
;; ═════════════════════════════════════════════════════════════════════════════

(use-package magit
  :bind (("C-c g" . magit-blame-addition))
  :custom
  (magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1)
  (magit-ediff-dwim-show-on-hunks t))

(use-package ediff
  :ensure nil                     ; built-in
  :defer t
  :custom
  (ediff-auto-refine 'on)
  (ediff-auto-refine-limit 10000)
  (ediff-window-setup-function #'ediff-setup-windows-plain)
  (ediff-split-window-function #'split-window-horizontally)
  :hook
  (ediff-startup . my/ediff-jump-to-first-difference)
  :init
  (defun my/ediff-jump-to-first-difference ()
    (ediff-jump-to-difference 1))

  (defun my/ediff-disable-y-or-n-p (orig-fun &rest args)
    "Auto-confirm `y-or-n-p' prompts while ORIG-FUN runs."
    (require 'cl-lib)
    (cl-letf (((symbol-function 'y-or-n-p) (lambda (_prompt) t)))
      (apply orig-fun args)))
  :config
  (setq-default ediff-forward-word-function #'forward-char)
  (advice-add 'ediff-quit :around #'my/ediff-disable-y-or-n-p))

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

;; multiple cursor
(use-package multiple-cursors
  :bind (("C-c n" . mc/mark-next-like-this)
	 ("C-c p" . mc/mark-previous-like-this)
	 ("C-c C-l" . mc/mark-all-like-this)))

(use-package ghostel
  :bind (("C-x m" . ghostel)
	 :map ghostel-semi-char-mode-map
	 ("C-s" . consult-line)
	 ("C-k" . my/ghostel-send-C-k-and-kill)
	 ("M-p" . (lambda () (interactive) (ghostel-send-key "p" "ctrl")))
	 ("M-n" . (lambda () (interactive) (ghostel-send-key "n" "ctrl"))))
  :config
  (setq ghostel-environment
      '("EDITOR=emacsclient"
	"VISUAL=emacsclient"))

  (defun my/ghostel-send-C-k-and-kill ()
    "Send `C-k' to ghostel
	Like normal Emacs `C-k'. Kill to end of line and put content in kill-ring"
    (interactive)
    (kill-ring-save (point) (line-end-position))
    (ghostel-send-key "k" "ctrl"))

  (add-to-list 'ghostel-eval-cmds '("magit-status-setup-buffer" magit-status-setup-buffer)))

(use-package project
  :ensure nil
  :bind (:map project-prefix-map
	      ("G" . magit-project-status)
              ("m" . ghostel-project)
              ("M" . ghostel-project-list-buffers))
  :config
  (assq-delete-all 'project-vc-dir project-switch-commands)
  (add-to-list 'project-switch-commands '(magit-project-status "Magit" ?G) t)
  (add-to-list 'project-switch-commands '(ghostel-project "Ghostel" ?m) t)
  (add-to-list 'project-switch-commands '(ghostel-project-list-buffers "Ghostel buffers" ?M) t))

(use-package consult-ghostel
  :hook (after-init . consult-ghostel-mode)
  :bind (("C-x m" . consult-ghostel)
         ;; :map project-prefix-map
         ;; ("m" . consult-ghostel-project)
         :map ghostel-semi-char-mode-map
         ("C-c h" . consult-ghostel-history)))

;; Flymake settings
(setq flymake-no-changes-timeout 0.2
      flymake-start-on-save-buffer t
      flymake-proc-allowed-file-name-masks nil) ;; don't analyze temp files

;; Disable excessive logging
(advice-add 'eglot--log-event :override #'ignore)

;; Eglot
(use-package eglot
  :hook ((eglot--managed-mode . breadcrumb-local-mode))
  :bind (:map eglot-mode-map ("<f2>" . eglot-rename)
	      ("<f6>" . eglot-format-buffer))
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
               `(simp-rust-mode . ,(eglot-alternatives '(("rust-analyzer")))))

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

(use-package python-ts-mode
  :ensure nil
  :hook ((python-ts-mode . display-fill-column-indicator-mode)
	 (python-ts-mode . eglot-ensure)
	 (python-ts-mode . ruff-check-on-save-mode)
	 (python-ts-mode . ruff-organize-imports-on-save-mode)
	 (python-ts-mode . ruff-format-on-save-mode)
	 (python-ts-mode . (lambda () (setq-local tab-width 4))))
  :mode (("\\.py\\'" . python-ts-mode)))

(use-package pyvenv
  :config
  (defun my/python-auto-venv ()
    "Activate the .venv belonging to the current project."
    (when-let* ((project (project-current nil))
                (root (project-root project))
                (venv (expand-file-name ".venv" root)))
      (when (and (file-directory-p venv)
                 (not (equal pyvenv-virtual-env venv)))
        (pyvenv-activate venv)
        (message "Activated venv: %s" venv))))

  (advice-add 'project-switch-project :after
              (lambda (&rest _)
                (my/python-auto-venv)))
  (add-hook 'python-ts-mode-hook #'my/python-auto-venv))


;; ─── Rust ────────────────────────────────────────────────────────────────────
(use-package simp-rust-mode
  :ensure nil
  :hook ((simp-rust-mode . display-fill-column-indicator-mode)
	 (simp-rust-mode . eglot-ensure)
	 (simp-rust-mode . (lambda () (setq-local tab-width 4))))
  :mode (("\\.rs\\'" . simp-rust-mode)))

;; ─── Markdown ──────────────────────────────────────────────────────────────
(use-package markdown-ts-mode
  :ensure nil
  :mode (("\\.md\\'" . markdown-ts-mode)))

(use-package markdown-ts-appear
  :pin nongnu
  :hook (markdown-ts-mode . markdown-ts-appear-mode))

(setopt markdown-ts-appear-diagram-renderers
        '(("mermaid" . markdown-ts-appear-diagram-mmdc)))

(use-package csv-mode
  :mode "\\.csv\\'")

;; ─── HTML ────────────────────────────────────────────────────────────────────
(use-package html-ts-mode
  :ensure nil
  :mode (("\\.html\\'" . html-ts-mode))
  :hook
  (html-ts-mode . (lambda ()
                    (setq-local tab-width 2)
                    (setq-local sgml-basic-offset 2)
                    (setq-local indent-tabs-mode nil))))

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

(use-package breadcrumb
  :vc (:url "https://github.com/caotanduc/breadcrumb"
	    :rev :newest))


(add-to-list 'load-path (expand-file-name "lisp/" user-emacs-directory))
;; Enable vscode-mode globally
(require 'vscode)
(global-vscode-mode 1)

;; Enable jumpy-mode globally
(require 'jumpy)
(jumpy-mode 1)

(global-set-key (kbd "C-c -")   #'jumpy-back)
(global-set-key (kbd "s-[")	#'jumpy-back)
(global-set-key (kbd "C-c _")	#'jumpy-forward)
(global-set-key (kbd "s-]")	#'jumpy-forward)

;; ═════════════════════════════════════════════════════════════════════════════
;;  THEME
;; ═════════════════════════════════════════════════════════════════════════════

(use-package github-dark-colorblind-theme
  :vc (:url "https://github.com/caotanduc/github-dark-colorblind-theme"
            :rev :newest))

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

(require 'server)
(unless (server-running-p) (server-start))

(setq major-mode-remap-alist
      '((python-mode . python-ts-mode)
	(js-json-mode . json-ts-mode)
	(sh-mode . bash-ts-mode)))

;; https://blog.alex.balgavy.eu/how-to-have-emacs-notify-you-when-it-needs-your-attention/
(defun my/notify (title message)
  "Show notification with TITLE and MESSAGE."
  (cond ((fboundp 'ns-do-applescript)
         (ns-do-applescript
          (format "display notification \"%s\" with title \"%s\""
                  (replace-regexp-in-string "\"" "#" message)
                  (replace-regexp-in-string "\"" "#" title))))
        (t (error "No notification handler defined!"))))
