(require 'cc-langs)

(defconst vlang-mode-keywords
  '("break" "const" "continue" "defer" "else" "enum" "fn" "for" "go" "goto" "if" "import" "in" "interface" "match" "module" "none" "or" "pub" "return" "struct" "type"))

(defconst vlang-mode-buildins
  '("mut"))

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
    (,(regexp-opt vlang-mode-types 'symbols) . font-lock-function-name-face)
    (,(regexp-opt vlang-mode-attributes) . font-lock-keyword-face)
    (,(regexp-opt vlang-mode-constants 'symbols) . font-lock-constant-face)

    ;; for commnet
    ("\\(`[^`]*`\\)" . font-lock-string-face)

    ;; for function name
    (".\\(\\_<\\(?:\\sw\\|\\s_\\)+?\\_>\\)\\s-*(" . (1 'font-lock-function-name-face))
    (".\\(\\_<\\(?:\\sw\\|\\s_\\)+?\\_>\\)\\s-*<[a-zA-Z0-9_]+>(" . (1 'font-lock-function-name-face))

    ;; for function type
    (".\\(\\_<\\(?:\\sw\\|\\s_\\)+?\\_>\\)\\s-*<[a-zA-Z0-9_]+>(" . (1 'font-lock-function-name-face))

    ;; for struct
    ("\\<struct\\s-+\\(\\sw+\\)" . (1 'font-lock-type-face))

    ;; for generics
    ("<\\(\\sw+\\)>" . (1 'font-lock-type-face))))

(defvar vlang-mode-syntax-table
  (let ((table (make-syntax-table)))
    (c-populate-syntax-table table)
    table))

(defun vlang-mode-completion-at-point ()
  (interactive)
  (let* ((bds (bounds-of-thing-at-point 'symbol))
         (start (car bds))
         (end (cdr bds)))
    (list start end (concatenate 'list vlang-mode-keywords vlang-mode-types vlang-mode-constants) . nil)))

;;;###autoload
(define-derived-mode vlang-mode prog-mode "Vlang"
  "Major mode for editing Vang"
  :syntax vlang-mode-syntax-table

  ;; indentation
  (setq indent-tabs-mode t)
  (setq tab-width 4)

  ;; font-lock
  (setq font-lock-multiline t)
  (setq font-lock-defaults '((vlang-mode-font-lock-keywords)))

  ;; hooks
  (add-hook 'completion-at-point-functions 'vlang-mode-completion-at-point nil 'local))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.v\\'"  . vlang-mode))

(provide 'vlang-mode)
