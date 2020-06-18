(defconst vlang-keywords
  '("break" "const" "continue" "defer" "else" "enum"
    "fn" "for" "go" "goto" "if" "import" "in" "interface"
    "match" "module" "mut" "none" "or" "pub" "return"
    "struct" "type" "var" "assert" "sizeof" "typeof"))

(defconst vlang-types
  '("bool" "string" "i8" "i16" "int" "i64" "i128" "byte"
    "u16" "u32" "u64" "u128" "rune" "f32" "f64" "size_t"
    "size_t" "char" "byteptr" "voidptr" "charptr" "any"
    "any_int" "any_float" "map"))

(defconst vlang-constants
  '("true" "false"))

(setq vlang-font-lock-keywords
      (let ((x-keywords-regexp   (regexp-opt vlang-keywords  'symbols))
             (x-types-regexp     (regexp-opt vlang-types     'symbols))
             (x-constants-regexp (regexp-opt vlang-constants 'symbols)))
        `((,x-keywords-regexp  . font-lock-keyword-face)
          (,x-types-regexp     . font-lock-type-face)
          (,x-constants-regexp . font-lock-constant-face))))

(defvar vlang-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?\/ ". 12b" table)
    (modify-syntax-entry ?\n "> b" table)
    table))

(defun vlang-completion-at-point ()
  (interactive)
  (let* ((bds (bounds-of-thing-at-point 'symbol))
         (start (car bds))
         (end (cdr bds)))
    (list start end (concatenate 'list vlang-keywords vlang-types vlang-constants) . nil )))

;;;###autoload
(define-derived-mode vlang-mode prog-mode "Vlang"
  "Major mode for editing Vang"
  :syntax vlang-mode-syntax-table
  (setq font-lock-defaults '((vlang-font-lock-keywords)))
  (add-hook 'completion-at-point-functions 'vlang-completion-at-point nil 'local))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.v\\'"  . vlang-mode))

(provide 'vlang-mode)
