;;; simp-rust-mode.el --- Minimal Rust mode without heavy syntax -*- lexical-binding: t; -*-

;; Author: Cao Tan Duc
;; Version: 0.1
;; Package-Requires: ((emacs "27.1"))
;; Keywords: languages, rust

;;; Commentary:
;; Minimal Rust major mode focused on speed and simplicity.

;;; Code:

(defgroup simp-rust nil
  "Minimal Rust editing mode."
  :group 'languages)

(defvar simp-rust-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c <") #'simp-rust-shift-left)
    (define-key map (kbd "C-c >") #'simp-rust-shift-right)
    map)
  "Keymap for `simp-rust-mode'.")

(defvar simp-rust-syntax-table
  (let ((st (make-syntax-table)))
    ;; C-style comments: // and /* */
    (modify-syntax-entry ?/ ". 124b" st)
    (modify-syntax-entry ?* ". 23" st)
    (modify-syntax-entry ?\n "> b" st)
    ;; Strings and characters
    (modify-syntax-entry ?\" "\"" st)
    (modify-syntax-entry ?\' "\"" st)
    ;; Braces
    (modify-syntax-entry ?\{ "(}" st)
    (modify-syntax-entry ?\} "){" st)
    st)
  "Syntax table for `simp-rust-mode'.")

(defun simp-rust-indent-line ()
  "Brace-based indentation. Moves cursor to start of text to avoid duplication."
  (interactive)
  (let ((indent 0))
    (save-excursion
      (beginning-of-line)
      ;; Count braces from top of buffer, ignoring strings/comments
      (save-excursion
        (while (re-search-backward "[{}]" nil t)
          (unless (nth 8 (syntax-ppss))
            (if (equal (match-string 0) "{")
                (setq indent (+ indent tab-width))
              (setq indent (- indent tab-width))))))
      ;; Check if current line starts with closing brace
      (back-to-indentation)
      (when (looking-at-p "}")
        (setq indent (- indent tab-width))))
    
    (indent-line-to (max 0 indent))
    (back-to-indentation)))

(defun simp-rust-shift-left (start end)
  "Shift the line or region to the left by `tab-width`."
  (interactive (if (use-region-p)
                   (list (region-beginning) (region-end))
                 (list (line-beginning-position) (line-end-position))))
  (indent-rigidly start end (- tab-width))
  (setq deactivate-mark nil))

(defun simp-rust-shift-right (start end)
  "Shift the line or region to the right by `tab-width`."
  (interactive (if (use-region-p)
                   (list (region-beginning) (region-end))
                 (list (line-beginning-position) (line-end-position))))
  (indent-rigidly start end tab-width)
  (setq deactivate-mark nil))

;;;###autoload
(define-derived-mode simp-rust-mode prog-mode "SimpRust"
  "A minimal, fast Rust mode without heavy syntax processing."
  :syntax-table simp-rust-syntax-table
  
  (setq-local comment-start "// ")
  (setq-local comment-end "")
  (setq-local indent-tabs-mode nil)
  
  ;; Indentation
  (setq-local indent-line-function #'simp-rust-indent-line)
  (setq-local tab-always-indent t)

  ;; Disable heavy features
  (setq-local font-lock-defaults nil)
  (font-lock-mode -1)
  (setq-local syntax-propertize-function nil)

  ;; Navigation (requires rust-mode to be installed as per your header)
  (when (fboundp 'rust-beginning-of-defun)
    (setq-local beginning-of-defun-function #'rust-beginning-of-defun)
    (setq-local end-of-defun-function #'rust-end-of-defun))

  ;; Electric behavior & Visuals
  (electric-indent-local-mode 1)
  (show-paren-local-mode 1)

  ;; Mode line tag
  (setq-local mode-line-format
              (append mode-line-format '((:eval " SimpRs")))))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.rs\\'" . simp-rust-mode))

(provide 'simp-rust-mode)
;;; simp-rust-mode.el ends here
