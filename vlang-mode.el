(defconst vlang-mode-keywords
  '("break" "const" "continue" "defer" "else" "enum"
    "fn" "for" "go" "goto" "if" "import" "in" "interface"
    "match" "module" "mut" "none" "or" "pub" "return"
    "struct" "type" "var" "assert" "sizeof" "typeof"))

(defconst vlang-mode-types
  '("bool" "string" "i8" "i16" "int" "i64" "i128" "byte"
    "u16" "u32" "u64" "u128" "rune" "f32" "f64" "size_t"
    "size_t" "char" "byteptr" "voidptr" "charptr" "any"
    "any_int" "any_float" "map"))

(defconst vlang-mode-constants
  '("true" "false"))

(defvar vlang-mode-font-lock-keywords
  `((,(regexp-opt vlang-mode-keywords 'symbols) . font-lock-keyword-face)
    (,(regexp-opt vlang-mode-types 'symbols) . font-lock-type-face)
    (,(regexp-opt vlang-mode-constants 'symbols) . font-lock-constant-face)
    ("\\(?:\\<fn\\s-+&?\\(\\(?:\\sw\\|\\s_\\)+\\)\\s-*(\\)" . font-lock-function-name-face)))

(defvar vlang-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?\/ ". 12b" table)
    (modify-syntax-entry ?\n "> b" table)
    table))

(defun vlang-mode-completion-at-point ()
  (interactive)
  (let* ((bds (bounds-of-thing-at-point 'symbol))
         (start (car bds))
         (end (cdr bds)))
    (list start end (concatenate 'list vlang-mode-keywords vlang-mode-types vlang-mode-constants) . nil )))

;;;###autoload
(define-derived-mode vlang-mode prog-mode "Vlang"
  "Major mode for editing Vang"
  :syntax vlang-mode-syntax-table
  (setq font-lock-defaults '((vlang-mode-font-lock-keywords)))
  (add-hook 'completion-at-point-functions 'vlang-mode-completion-at-point nil 'local))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.v\\'"  . vlang-mode))

(provide 'vlang-mode)
