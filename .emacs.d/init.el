(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

(setq inhibit-startup-screen t)
(setq visible-bell t)

(add-to-list 'default-frame-alist `(font . "Iosevka Extended-20"))

(add-to-list 'initial-frame-alist '(fullscreen . maximized))
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Remove title bar / window decorations
(add-to-list 'initial-frame-alist '(undecorated . t))
(add-to-list 'default-frame-alist '(undecorated . t))

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; editor config
(electric-pair-mode t)

(setq-default display-fill-column-indicator-column 79)

(set-face-attribute 'fill-column-indicator nil :background 'unspecified :foreground "grey90")

(setq tab-always-indent 'complete)
(setq eshell-destroy-buffer-when-process-dies t)

(setq custom-file (expand-file-name ".emacs.custom.el" user-emacs-directory))
(load custom-file)

(let ((data-dir (expand-file-name "tmp/" user-emacs-directory)))
  (make-directory data-dir t)
  (setq backup-directory-alist `(("." . ,(expand-file-name "backups/" data-dir))))
  (setq auto-save-list-file-prefix (expand-file-name "auto-saves/sessions/" data-dir))
  (setq auto-save-file-name-transforms `((".*", (expand-file-name "auto-saves" data-dir) t))))

(let ((data-dir (expand-file-name "var/" user-emacs-directory)))
  (make-directory data-dir t)
  (setq auto-save-list-file-prefix (expand-file-name "auto-save/" data-dir))
  (setq eshell-directory-name (expand-file-name "eshell/" data-dir))
  (setq transient-history-file (expand-file-name "transient/history.el" data-dir))
  (setq package-user-dir (expand-file-name "elpa/" data-dir)))

;; Packages
(setq use-package-always-defer t)

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)
(add-to-list 'package-archives '("elpa" . "https://elpa.gnu.org/packages/") t)

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;; Enable Vertico.
(use-package vertico
  :custom
  (vertico-resize t)
  (vertico-cycle t)
  :init
  (vertico-mode)
  (vertico-insert))

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :init
  (savehist-mode))

;; Allow nested minibuffers
(setq enable-recursive-minibuffers t)

;; Hide commands in M-x that don’t work in current mode
(setq read-extended-command-predicate #'command-completion-default-include-p)

;; Make minibuffer prompt read-only and non-editable
(setq minibuffer-prompt-properties
      '(read-only t cursor-intangible t face minibuffer-prompt))

(use-package orderless
  :custom
  (completion-styles '(partial-completion flex))
  (completion-category-overrides '((file (styles partial-completion))))
  (completion-category-defaults nil) ;; Disable defaults, use our settings
  (completion-pcm-leading-wildcard t)) ;; Emacs 31: partial-completion behaves like substring

(use-package marginalia
  :init
  (marginalia-mode))

;; Example configuration for Consult
(use-package consult
  :bind (;; C-x bindings in `ctl-x-map'
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ;; M-g bindings in `goto-map'
         ("M-g e" . consult-compile-error)
         ("M-g r" . consult-grep-match)
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings in `search-map'
         ("M-s d" . consult-find)                  ;; Alternative: consult-fd
         ("M-s c" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
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
   :preview-key '(:debounce 0.4 any))
)

(with-eval-after-load 'eshell
  (define-key eshell-mode-map (kbd "s-r") #'consult-history))

;; -----------------------------
;; Project-aware Eshell workflow
;; -----------------------------

(defun my/project-eshell-buffer-name ()
  "Return an Eshell buffer name based on the current project."
  (if-let ((proj (project-current)))
      (format "*eshell-%s*" (project-name proj))
    "*eshell*"))

(defun my/project-or-global-eshell ()
  "Open project Eshell if in a project, otherwise reuse global Eshell.
Reuses an existing Eshell buffer if available."
  (interactive)
  (let* ((name (my/project-eshell-buffer-name))
         (buf (get-buffer name)))
    (if buf
        (switch-to-buffer buf)
      (if-let ((proj (project-current)))
          (project-eshell)
        (eshell)))))

(defun my/project-eshell-new ()
  "Always open a new Eshell buffer for the current project."
  (interactive)
  (let ((default-directory
          (if-let ((proj (project-root (project-current))))
              proj
            default-directory))
        (eshell-buffer-name (generate-new-buffer-name (my/project-eshell-buffer-name))))
    (eshell)))

;; Keybindings
(global-set-key (kbd "C-`") #'my/project-or-global-eshell)
(global-set-key (kbd "C-~") #'my/project-eshell-new)

(use-package magit)

(use-package multiple-cursors)

(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

(when (and (eq system-type 'darwin) window-system)
  (setq exec-path (append exec-path '("/opt/homebrew/bin/"
                                      "~/.cargo/bin/"
                                      "/usr/local/bin/"))))

(use-package expand-region
  :bind (("C-=" . er/expand-region)))

(use-package breadcrumb)

(use-package company
  :init
  (global-company-mode))

(setq treesit-language-source-alist
      '((python "https://github.com/tree-sitter/tree-sitter-python" "v0.23.6")
	(yaml "https://github.com/tree-sitter-grammars/tree-sitter-yaml" "v0.7.2")))

(use-package eglot
  :hook ((eglot--managed-mode . breadcrumb-local-mode)
         (eglot--managed-mode . eglot-booster-mode))
  :bind (:map eglot-mode-map ("<f2>" . eglot-rename))
  :config
  (customize-set-variable 'eglot-events-buffer-config '(:size 0))
  (fset #'jsonrpc--log-event #'ignore)
  (setq jsonrpc-event-hook nil)
  ;; Run both basedpyright and ruff for python-ts-mode
  (add-to-list 'eglot-server-programs
               `(python-ts-mode . ,(eglot-alternatives '(("pyright-langserver" "--stdio")
                                                         ("basedpyright-langserver" "--stdio")
                                                         ("ruff" "server")))))

  ;; Configure basedpyright and inlay hints
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

(use-package yaml-ts-mode
  :ensure nil
  :mode (("\\.yaml\\'" . yaml-ts-mode)
	 ("\\.yml\\'" . yaml-ts-mode)))

(use-package python-ts-mode
  :ensure nil
  :hook ((python-ts-mode . display-fill-column-indicator-mode)
	 (python-ts-mode . eglot-ensure)
         (python-ts-mode . conda-env-autoactivate-mode)
         (python-ts-mode . ruff-check-on-save-mode)
         (python-ts-mode . ruff-organize-imports-on-save-mode)
         (python-ts-mode . ruff-format-on-save-mode))
  :mode (("\\.py\\'" . python-ts-mode))
  :config
  (setq indent-tabs-mode nil)
  (setq tab-width 4)
  (setq electric-indent-inhibit nil)
  (electric-indent-mode 1))

(use-package conda
  :config
  (conda-env-initialize-eshell)
  (conda-env-initialize-interactive-shells)
  (conda-env-autoactivate-mode t)
  
  (conda-mode-line-setup)
  (custom-set-variables
   '(conda-anaconda-home "~/miniconda3/")
   '(conda-env-home-directory "~/miniconda3/")))

(use-package git-gutter)
(global-git-gutter-mode +1)

(use-package vterm
  :init
  (setq vterm-kill-buffer-on-exit t))

(use-package eat
  :init
  (setq eat-kill-buffer-on-exit t))

(use-package esh-autosuggest
  :hook (eshell-mode . esh-autosuggest-mode))

;; For `eat-eshell-mode'.
(add-hook 'eshell-load-hook #'eat-eshell-mode)

;; For `eat-eshell-visual-command-mode'.
(add-hook 'eshell-load-hook #'eat-eshell-visual-command-mode)


(add-to-list 'load-path (expand-file-name "lisp/" user-emacs-directory))
(require 'eglot-booster)
(require 'vscode)
(require 'dashboard)
(require 'jumpy)

(jumpy-mode 1)

(global-set-key (kbd "C--")   #'jumpy-back)
(global-set-key (kbd "C-_") #'jumpy-forward)

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(when window-system
  (load-theme 'github-dark-colorblind t))

(global-vscode-mode 1)
(add-hook 'emacs-startup-hook #'my/welcome-buffer)

(require 'magit)
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
   (:eval
    (let ((branch (magit-get-current-branch)))
      (when branch
	(format " %s " branch))))
   (project-mode-line project-mode-line-format)
   ;; Flymake
   (:eval (when (bound-and-true-p flymake-mode)
            '(" " flymake-mode-line-counters)))
   ;; Align right
   mode-line-format-right-align
   "Ln: %l "
   (:eval
    (when (and (boundp 'conda-env-current-name)
               conda-env-current-name)
      (concat "[py:" conda-env-current-name "]")))
   mode-line-end-spaces))
