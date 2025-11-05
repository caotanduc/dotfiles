;;; github-dark-colorblind-theme.el --- GitHub Dark Colorblind theme for Emacs -*- lexical-binding: t; -*-

;; Author: Converted by ChatGPT
;; Version: 1.0
;; License: MIT

(deftheme github-dark-colorblind
  "Dark theme variant of GitHub theme, color-blind friendly.")

(let ((class '((class color) (min-colors 89)))
      ;; Base colors (derived from Primer / GitHub theme “Dark Colorblind” variant)
      (bg         "#0d1117")  ;; main background
      (bg-alt     "#161b22")  ;; alternate background
      (fg         "#c9d1d9")  ;; primary foreground
      (fg-muted   "#8b949e")  ;; muted foreground
      (accent     "#56d364")  ;; accent (green)
      (keyword    "#f47067")  ;; keyword (salmon)
      (func       "#a5d6ff")  ;; function name (light blue)
      (type       "#ffab70")  ;; type (orange)
      (string     "#7ee787")  ;; string (light green)
      (comment    "#8c959f")  ;; comment (gray)
      (punct      "#f0f6fc")  ;; punctuation/lighter
      (error      "#ff7b72")  ;; error (red)
      (warning    "#e3b341")  ;; warning (yellow)
      )

  (custom-theme-set-faces
   'github-dark-colorblind

   ;; Basics
   `(default             ((,class (:background ,bg :foreground ,fg))))
   `(cursor              ((,class (:background ,fg))))
   `(fringe              ((,class (:background ,bg))))
   `(region              ((,class (:background ,bg-alt))))
   `(highlight           ((,class (:background ,bg-alt :foreground ,fg))))
   `(minibuffer-prompt   ((,class (:foreground ,keyword :weight bold))))
   `(link                ((,class (:foreground ,type :underline t))))

   ;; Font lock (syntax)
   `(font-lock-builtin-face          ((,class (:foreground ,keyword))))
   `(font-lock-comment-face          ((,class (:foreground ,comment :slant italic))))
   `(font-lock-doc-face              ((,class (:inherit font-lock-comment-face :foreground ,fg-muted))))
   `(font-lock-keyword-face          ((,class (:foreground ,keyword :weight bold))))
   `(font-lock-function-name-face    ((,class (:foreground ,func))))
   `(font-lock-variable-name-face    ((,class (:foreground ,fg))))
   `(font-lock-type-face              ((,class (:foreground ,type :weight bold))))
   `(font-lock-constant-face          ((,class (:accent ,accent))))
   `(font-lock-string-face            ((,class (:foreground ,string))))
   `(font-lock-number-face            ((,class (:foreground ,string))))
   `(font-lock-punctuation-face       ((,class (:foreground ,punct))))
   `(font-lock-warning-face           ((,class (:foreground ,warning :weight bold))))
   `(error-face                     ((,class (:foreground ,error :weight bold))))

   ;; Mode line
   `(mode-line
     ((,class (:background ,bg-alt :foreground ,fg :box (:line-width -1 :color ,bg-alt) :height 0.9))))
   `(mode-line-inactive
     ((,class (:background ,bg :foreground ,fg-muted :box (:line-width -1 :color ,bg) :height 0.9))))
   `(mode-line-buffer-id
     ((,class (:foreground ,type :weight bold :height 0.9))))

   ;; Header line (if you use it)
   `(header-line
     ((,class (:background ,bg-alt :foreground ,fg :box (:line-width -1 :color ,bg-alt) :height 0.9))))

   ;; Vertical border
   `(vertical-border ((,class (:foreground ,bg-alt))))

   ;; Additional UI
   `(tooltip ((,class (:background ,bg-alt :foreground ,fg :height 0.9))))
   `(region ((,class (:background ,bg-alt))))

   ;; Add more faces as needed for your packages…

   ))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'github-dark-colorblind)
;;; github-dark-colorblind-theme.el ends here
