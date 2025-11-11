;;; github-dark-colorblind-theme.el --- GitHub Dark Colorblind theme for Emacs -*- lexical-binding: t; -*-

;; Author: Converted and extended by ChatGPT
;; Version: 1.1
;; License: MIT

(deftheme github-dark-colorblind
  "Dark theme variant of GitHub theme, color-blind friendly.")

(let ((class '((class color) (min-colors 89)))
      ;; Base colors (from GitHub Dark Colorblind)
      (bg         "#0d1117")
      (bg-alt     "#161b22")
      (bg-hover   "#1f242c")
      (fg         "#c9d1d9")
      (fg-muted   "#8b949e")
      (accent     "#56d364")  ;; green
      (keyword    "#f47067")  ;; salmon
      (func       "#a5d6ff")  ;; light blue
      (type       "#ffab70")  ;; orange
      (string     "#7ee787")  ;; light green
      (comment    "#8c959f")
      (punct      "#f0f6fc")
      (error      "#ff7b72")
      (warning    "#e3b341")
      (info       "#79c0ff")
      (highlight  "#2d333b"))

  (custom-theme-set-faces
   'github-dark-colorblind

    ;; Flat mode line
    `(mode-line
      ((,class (:background ,bg-alt :foreground ,fg
                            :box nil           ;; remove border
                            :overline nil      ;; remove top line
                            :underline nil     ;; remove bottom line
                            :weight normal
                            :height 0.95))))

    `(mode-line-inactive
      ((,class (:background ,bg :foreground ,fg-muted
                            :box nil
                            :overline nil
                            :underline nil
                            :height 0.95))))

    ;; Flat header line / breadcrumb
    `(header-line
      ((,class (:background ,bg-alt :foreground ,fg
                            :box nil
                            :overline nil
                            :underline nil
                            :height 0.95))))

    ;; Breadcrumb (if used)
    `(breadcrumb-face
      ((,class (:background ,bg-alt :foreground ,fg-muted
                            :box nil
                            :overline nil
                            :underline nil))))

   ;; ───── Basic UI ─────
   `(default             ((,class (:background ,bg :foreground ,fg))))
   `(cursor              ((,class (:background ,fg))))
   `(fringe              ((,class (:background ,bg))))
   `(region              ((,class (:background ,highlight))))
   `(highlight           ((,class (:background ,bg-alt :foreground ,fg))))
   `(link                ((,class (:foreground ,type :underline t))))
   `(minibuffer-prompt   ((,class (:foreground ,keyword :weight bold))))
   `(vertical-border     ((,class (:foreground ,bg-alt))))
   `(shadow              ((,class (:foreground ,fg-muted))))

   ;; ───── Syntax ─────
   `(font-lock-builtin-face          ((,class (:foreground ,keyword))))
   `(font-lock-comment-face          ((,class (:foreground ,comment :slant italic))))
   `(font-lock-doc-face              ((,class (:foreground ,fg-muted :slant italic))))
   `(font-lock-keyword-face          ((,class (:foreground ,keyword :weight bold))))
   `(font-lock-function-name-face    ((,class (:foreground ,func))))
   `(font-lock-variable-name-face    ((,class (:foreground ,fg))))
   `(font-lock-type-face             ((,class (:foreground ,type :weight bold))))
   `(font-lock-constant-face         ((,class (:foreground ,accent))))
   `(font-lock-string-face           ((,class (:foreground ,string))))
   `(font-lock-number-face           ((,class (:foreground ,string))))
   `(font-lock-punctuation-face      ((,class (:foreground ,punct))))
   `(font-lock-warning-face          ((,class (:foreground ,warning :weight bold))))
   `(error                           ((,class (:foreground ,error :weight bold))))

   ;; ───── Mode line ─────
   `(mode-line
     ((,class (:background ,bg-alt :foreground ,fg
                           :box (:line-width -1 :color ,bg-hover)
                           :overline ,bg-hover :underline ,bg-hover
                           :weight normal :height 0.95))))
   `(mode-line-inactive
     ((,class (:background ,bg :foreground ,fg-muted
                           :box (:line-width -1 :color ,bg)
                           :height 0.95))))
   `(mode-line-buffer-id
     ((,class (:foreground ,type :weight bold))))

   ;; ───── Breadcrumb (lsp-headerline, eglot, etc.) ─────
   `(breadcrumb-face                 ((,class (:foreground ,fg-muted :background ,bg-alt))))
   `(breadcrumb-symbols-face         ((,class (:foreground ,fg :background ,bg-alt))))
   `(breadcrumb-project-face         ((,class (:foreground ,accent :weight bold))))
   `(breadcrumb-file-face            ((,class (:foreground ,type))))
   `(breadcrumb-key-face             ((,class (:foreground ,keyword))))

   ;; ───── Vertico / Marginalia / Orderless ─────
   `(vertico-current                 ((,class (:background ,bg-hover :foreground ,fg :weight bold))))
   `(vertico-group-title             ((,class (:foreground ,keyword :weight bold))))
   `(vertico-group-separator         ((,class (:foreground ,bg-alt :strike-through t))))
   `(marginalia-documentation        ((,class (:foreground ,fg-muted :slant italic))))
   `(orderless-match-face-0          ((,class (:foreground ,accent :weight bold))))
   `(orderless-match-face-1          ((,class (:foreground ,func :weight bold))))
   `(orderless-match-face-2          ((,class (:foreground ,keyword :weight bold))))
   `(orderless-match-face-3          ((,class (:foreground ,type :weight bold))))

   ;; ───── Corfu completion ─────
   `(corfu-default                   ((,class (:background ,bg-alt :foreground ,fg))))
   `(corfu-border                    ((,class (:background ,bg-hover))))
   `(corfu-current                   ((,class (:background ,bg-hover :foreground ,fg :weight bold))))
   `(corfu-bar                       ((,class (:background ,accent))))

   ;; ───── Which-key ─────
   `(which-key-key-face              ((,class (:foreground ,accent :weight bold))))
   `(which-key-group-description-face((,class (:foreground ,keyword))))
   `(which-key-command-description-face ((,class (:foreground ,fg))))
   `(which-key-separator-face        ((,class (:foreground ,fg-muted))))

   ;; ───── Consult / Isearch ─────
   `(consult-preview-match           ((,class (:background ,bg-hover))))
   `(isearch                         ((,class (:background ,accent :foreground ,bg :weight bold))))
   `(lazy-highlight                  ((,class (:background ,bg-hover :foreground ,accent))))
   `(match                           ((,class (:background ,bg-hover :foreground ,accent :weight bold))))

   ;; ───── Dired ─────
   `(dired-directory                 ((,class (:foreground ,func :weight bold))))
   `(dired-flagged                   ((,class (:foreground ,error))))
   `(dired-header                    ((,class (:foreground ,keyword :weight bold))))
   `(dired-ignored                   ((,class (:foreground ,fg-muted))))
   `(dired-mark                      ((,class (:foreground ,accent :weight bold))))
   `(dired-perm-write                ((,class (:foreground ,warning))))
   `(dired-symlink                   ((,class (:foreground ,type))))
   `(dired-warning                   ((,class (:foreground ,warning :weight bold))))

   ;; ───── Diff / Git ─────
   `(diff-added                      ((,class (:background "#12261a" :foreground "#56d364"))))
   `(diff-removed                    ((,class (:background "#2c181a" :foreground "#ff7b72"))))
   `(diff-changed                    ((,class (:background "#1f232a" :foreground "#e3b341"))))
   `(diff-refine-added               ((,class (:background "#17301f" :foreground "#7ee787"))))
   `(diff-refine-removed             ((,class (:background "#3b1f22" :foreground "#ffaba3"))))
   `(diff-hl-insert                  ((,class (:foreground ,accent :background "#17301f"))))
   `(diff-hl-delete                  ((,class (:foreground ,error :background "#3b1f22"))))
   `(diff-hl-change                  ((,class (:foreground ,warning :background "#2a2200"))))

   ;; ───── Highlight-symbol / current line ─────
   `(highlight-symbol-face           ((,class (:background ,bg-hover))))
   `(hl-line                         ((,class (:background ,bg-alt))))

   ;; ───── Misc ─────
   `(success                         ((,class (:foreground ,accent :weight bold))))
   `(warning                         ((,class (:foreground ,warning :weight bold))))
   `(error                           ((,class (:foreground ,error :weight bold))))
   `(tooltip                         ((,class (:background ,bg-alt :foreground ,fg))))
   `(show-paren-match                ((,class (:background ,bg-hover :foreground ,accent :weight bold))))
   `(show-paren-mismatch             ((,class (:background ,error :foreground ,bg))))

   ))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'github-dark-colorblind)
;;; github-dark-colorblind-theme.el ends here
