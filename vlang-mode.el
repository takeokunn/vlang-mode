(defconst vlang-mode-keywords
  '("break" "const" "continue" "defer" "else" "enum" "fn" "for" "go" "goto" "if" "import" "in" "interface" "match" "module" "none" "or" "pub" "return" "struct" "type"))

(defconst vlang-mode-buildins
  '("mut" "print" "println"))

(defconst vlang-mode-types
  '("bool"
    "string"
    "i8" "i16" "int" "i64" "i128"
    "byte" "u16" "u32" "u64" "u128"
    "rune"
    "f32" "f64"
    "any_int" "any_float"
    "byteptr" "voidptr" "charptr" "size_t"
    "any"))

(defconst vlang-mode-attributes
  '("deprecated" "inline" "typedef"))

(defconst vlang-mode-constants
  '("true" "false"))

(defvar vlang-mode-font-lock-keywords
  `((,(regexp-opt vlang-mode-keywords 'symbols) . font-lock-keyword-face)
    (,(regexp-opt vlang-mode-buildins 'symbols) . font-lock-keyword-face)
    (,(regexp-opt vlang-mode-types 'symbols) . font-lock-type-face)
    (,(regexp-opt vlang-mode-attributes) . font-lock-keyword-face)
    (,(regexp-opt vlang-mode-constants 'symbols) . font-lock-constant-face)
    ("\\(?:\\<fn\\s-+&?\\(\\(?:\\sw\\|\\s_\\)+\\)\\s-*(\\)" . (1 'font-lock-function-name-face))
    ("\\(\\_<\\(?:\\sw\\|\\s_\\)+?\\_>\\)\\s-*(" . (1 'font-lock-function-name-face))
    ("\\(`[^`]*`\\)" . font-lock-string-face)
    ("\\('[^']*'\\)" . font-lock-string-face)))

(defvar vlang-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?\/ ". 12b" table)
    (modify-syntax-entry ?\n "> b" table)
    (modify-syntax-entry ?\" "\"" table)
    (modify-syntax-entry ?\' "\"" table)
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

  ;; font-lock
  (setq font-lock-multiline t)
  (setq font-lock-defaults '((vlang-mode-font-lock-keywords)))

  ;; hooks
  (add-hook 'completion-at-point-functions 'vlang-mode-completion-at-point nil 'local))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.v\\'"  . vlang-mode))

(provide 'vlang-mode)
