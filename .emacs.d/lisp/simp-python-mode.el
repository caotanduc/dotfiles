;;; simp-python-mode.el --- Minimal Python mode without heavy syntax -*- lexical-binding: t; -*-

;; Author: Cao Tan Duc
;; Version: 0.1
;; Keywords: languages, python

;;; Commentary:
;; A minimal Python major mode derived from prog-mode.
;; No heavy background parsing, no tree-sitter, just fast editing.

;;; Code:

(defgroup simp-python nil
  "Minimal Python editing mode."
  :group 'languages)

(defvar simp-python-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c <") #'simp-python-shift-left)
    (define-key map (kbd "C-c >") #'simp-python-shift-right)
    map)
  "Keymap for `simp-python-mode'.")

(defvar simp-python-syntax-table
  (let ((st (make-syntax-table)))
    ;; Comments and Strings
    (modify-syntax-entry ?# "<" st)   ; # starts a comment
    (modify-syntax-entry ?\n ">" st)  ; Newline ends a comment
    (modify-syntax-entry ?\' "\"" st) ; Single quote
    (modify-syntax-entry ?\" "\"" st) ; Double quote
    ;; Word constituents (important for navigation and underscores)
    (modify-syntax-entry ?_ "w" st)
    ;; Parentheses and Brackets
    (modify-syntax-entry ?\( "()" st)
    (modify-syntax-entry ?\) ")(" st)
    (modify-syntax-entry ?\[ "(]" st)
    (modify-syntax-entry ?\] ")[" st)
    st)
  "Syntax table for `simp-python-mode'.")

(defun simp-python-indent-line ()
  "Simple indentation for Python. 
Calculates indent based on colon (:) suffix and moves cursor to start of text."
  (interactive)
  (let ((indent 0))
    (save-excursion
      ;; 1. Find previous non-blank line
      (forward-line -1)
      (while (and (looking-at "^[[:space:]]*$") (not (bobp)))
        (forward-line -1))
      (setq indent (current-indentation))
      
      ;; 2. If previous line ends in ':', increase indent
      (back-to-indentation)
      (let ((eol (line-end-position)))
        (save-excursion
          (while (re-search-forward ":" eol t)
            (unless (nth 8 (syntax-ppss)) ;; Skip if inside string/comment
              (setq indent (+ indent tab-width)))))))

    ;; 3. Adjust current line for 'dedent' keywords
    (save-excursion
      (beginning-of-line)
      (back-to-indentation)
      (when (looking-at-p "\\(else\\|elif\\|except\\|finally\\):")
        (setq indent (max 0 (- indent tab-width)))))

    ;; 4. Apply and move cursor to text (prevents duplication bug)
    (indent-line-to (max 0 indent))
    (back-to-indentation)))

(defun simp-python-shift-left (start end)
  "Shift the line or region left by `tab-width`."
  (interactive (if (use-region-p)
                   (list (region-beginning) (region-end))
                 (list (line-beginning-position) (line-end-position))))
  (indent-rigidly start end (- tab-width))
  (setq deactivate-mark nil))

(defun simp-python-shift-right (start end)
  "Shift the line or region right by `tab-width`."
  (interactive (if (use-region-p)
                   (list (region-beginning) (region-end))
                 (list (line-beginning-position) (line-end-position))))
  (indent-rigidly start end tab-width)
  (setq deactivate-mark nil))

;;;###autoload
(define-derived-mode simp-python-mode prog-mode "SimpPython"
  "A minimal, fast Python mode derived from prog-mode."
  :syntax-table simp-python-syntax-table
  :keymap simp-python-mode-map

  ;; Basic text rules
  (setq-local comment-start "# ")
  (setq-local comment-end "")
  (setq-local indent-tabs-mode nil)

  ;; Indentation
  (setq-local indent-line-function #'simp-python-indent-line)
  (setq-local tab-always-indent t)

  ;; Performance: Disable heavy built-ins
  (setq-local font-lock-defaults nil)
  (font-lock-mode -1)
  (setq-local syntax-propertize-function nil)
  (setq-local imenu-create-index-function nil)

  ;; Fast Navigation (Regex based)
  (setq-local beginning-of-defun-function
              (lambda () (re-search-backward "^[[:space:]]*\\(def\\|class\\) " nil t)))
  (setq-local end-of-defun-function
              (lambda () (re-search-forward "^[[:space:]]*\\(def\\|class\\) " nil t)))

  ;; Visuals
  (electric-indent-local-mode 1)
  (show-paren-local-mode 1)

  ;; Mode line indicator
  (setq-local mode-line-format
              (append mode-line-format '((:eval " SimpPy")))))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.py\\'" . simp-python-mode))

(provide 'simp-python-mode)
;;; simp-python-mode.el ends here
