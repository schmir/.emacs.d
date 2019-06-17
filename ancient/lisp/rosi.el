;;; rosi.el --- A major mode for editing ROSI files
;;
;;; Commentary:
;;
;; This is a simple mode for rosi sql.
;;
;;; Code:


(defgroup rosi nil
  "Major mode for editing Rosi code."
  :prefix "rosi-"
  :group 'languages
  :link '(url-link :tag "Github" "https://github.com/rosi-emacs/rosi-mode")
  :link '(emacs-commentary-link :tag "Commentary" "rosi-mode"))

(defface rosi-perform-face
  '((t (:inherit font-lock-warning-face)))
  "Face used to font-lock the function calls."
  :group 'rosi
  :package-version '(rosi-mode . "1.0.0"))

(defface rosi-assign-face
  '((t (:inherit font-lock-warning-face)))
  "Face used to font-lock assign statements."
  :group 'rosi
  :package-version '(rosi-mode . "1.0.0"))

(defface rosi-control-face
  '((t (:inherit font-lock-preprocessor-face)))
  "Face used to font-lock break/return/continue statements."
  :group 'rosi
  :package-version '(rosi-mode . "1.0.0"))

(defface rosi-field-face
  '((t (:inherit font-lock-preprocessor-face)))
  "Face used to font-lock field statements."
  :group 'rosi
  :package-version '(rosi-mode . "1.0.0"))

;; Mode variables
(defvar rosi-mode-hook nil
  "Hook run when we enter `rosi-mode'.")


(defvar rosi-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-j" 'newline-and-indent)
    map)
  "Keymap for rosi major mode.")

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.rsf\\|\\.rsi\\'" . rosi-mode))


(defvar rosi-font-lock-keywords
  `(("^//.*$" . font-lock-comment-face)
    ("^#.*" . font-lock-preprocessor-face)
    ("^[_a-zA-Z]+:" . font-lock-constant-face)
    (,(rx symbol-start
	  (or "fn_acos" "fn_asin" "fn_atan" "fn_atan2" "fn_cos" "fn_exp"
	      "fn_frac" "fn_hypot" "fn_log" "fn_log10" "fn_power"
	      "fn_sin" "fn_sqrt" "fn_tan" "abs" "clipped" "getchar"
	      "putchar" "instring" "matches" "picture" "strlen" "cexpand" "downshift")
	  symbol-end) . font-lock-builtin-face)
    (,(rx symbol-start
	  (or "char" "smallint" "integer" "date" "smallfloat" "float" "decimal"
	      "money" "channel" "datetime" "interval")
	  symbol-end) . font-lock-type-face)
    (,(rx symbol-start "goto" (1+ space) (group (1+ (or word ?_)))) (1 font-lock-constant-face))
    (,(rx symbol-start (or "proc" "procedure") (1+ space) (group (1+ (or word ?_))))
     (1 font-lock-function-name-face))

    (,(rx symbol-start (or "let" "init" "returning") symbol-end) . 'rosi-assign-face)

    (,(rx symbol-start (or "return" "break" "continue") symbol-end)  . 'rosi-control-face)

    (,(rx symbol-start (or "perform" "call") symbol-end) . 'rosi-perform-face)

    (,(rx symbol-start (or "field") symbol-end) . 'rosi-field-face)

    (, (regexp-opt
       '("abort" "abortstatus" "abs" "absolute" "actions" "after" "and"
	 "append" "appevent" "arg" "argcount" "at" "attribute" "auto"
	 "autonext" "avg" "background" "before" "bell" "binary" "black"
	 "blankfill" "blink" "blue" "bordered" "breakpoint"
	 "call" "case" "catoffset" "catstatus" "cattable" "catuser"
	 "center" "cexpand" "chain" "channel" "chaoffset" "char"
	 "chdir" "child" "clipped" "close" "coldim" "coloff" "colpos"
	 "comment" "compute" "concat" "concatenated" "constant"
	 "control" "converror" "copy" "core" "count"
	 "create" "current" "cursor" "cyan" "data" "database" "date"
	 "datetime" "day" "dayerror" "decimal" "default" "define"
	 "delete" "delimiter" "dialog" "dimension" "display"
	 "displayonly" "down" "downshift" "dynamic" "else" "elseif"
	 "end" "enter" "environment" "erase" "errortext" "event"
	 "every" "exception" "execute" "exitcode" "export" "fatal"
	 "fetch" "file" "first" "float" "floaterror" "flush"
	 "fn_acos" "fn_asin" "fn_atan" "fn_atan2" "fn_cos" "fn_exp"
	 "fn_frac" "fn_hypot" "fn_log" "fn_log10" "fn_power" "fn_sin"
	 "fn_sqrt" "fn_tan" "for" "fork" "form" "formfeed" "fraction"
	 "from" "function" "getchar" "getstr" "gettime" "goto" "green"
	 "group" "header" "help" "highlight" "history" "hold" "hour"
	 "id" "if" "illegalop" "import" "inkey" "insert"
	 "instring" "int" "integer" "interrupt" "interval" "into"
	 "intrstatus" "is" "isnumeric" "key" "key1" "key10" "key11"
	 "key12" "key13" "key14" "key15" "key16" "key17" "key18"
	 "key19" "key2" "key20" "key21" "key22" "key23" "key24" "key25"
	 "key26" "key27" "key28" "key29" "key3" "key30" "key4" "key5"
	 "key6" "key7" "key8" "key9" "keycr" "keycrclr" "keydel"
	 "keydelchr" "keydown" "keyend" "keyerase" "keyesc" "keyhelp"
	 "keyhome" "keyins" "keyinschr" "keyleft" "keynext" "keyprev"
	 "keyright" "keystatus" "keytab" "keyup" "killsqlexec" "last"
	 "lastcolpos" "leapyear" "left" "leftmargin" "length"
	 "like" "lindim" "line" "linecounter" "link" "linoff"
	 "location" "lock" "magenta" "maske" "matches" "matrix" "max"
	 "menu" "menue" "message" "min" "minute" "mode" "module"
	 "money" "month" "move" "multiple" "need" "newsqlexec" "next"
	 "nextfield" "nextpage" "noerase" "nolinefeed" "nonblocking"
	 "noobject" "normal" "not" "null" "of" "off" "offset" "on"
	 "open" "or" "otherwise" "overflow" "pagenumber" "parameter"
	 "parent" "path" "pause" "phycoldim" "phylindim"
	 "picture" "pipe" "pmode0" "pmode1" "pmode2" "pmode3" "pmode4"
	 "pmode5" "pmode6" "pmode7" "pmode8" "pmode9" "pos" "position"
	 "prepare" "previous" "print" "printername" "prior" "proc"
	 "procedure" "put" "putchar" "putfield" "putstr" "query"
	 "range" "read" "red" "refresh" "relative" "release" "removed"
	 "repeat" "report" "required" "retry"
	 "reverse" "right" "rightjust" "root" "round" "row" "run"
	 "scrcoldim" "scrcoloff" "screen" "screeninit" "screenreset"
	 "screndcol" "screndlin" "scrlindim" "scrlinoff" "scroll"
	 "second" "secret" "section" "seek" "select" "separator" "set"
	 "setlock" "shell" "show" "skip" "smallfloat" "smallint" "sql"
	 "sqlcmd" "sqlcount" "sqlerror" "sqllen" "sqlname" "sqlnull"
	 "sqlstatus" "sqltype" "sqlvalue" "sqlwarning" "status" "step"
	 "stop" "strlen" "sum" "switch" "sysbreak" "sysdate" "syserror"
	 "sysfatal" "sysinfo" "syskey" "syspid" "sysppid" "syssignal"
	 "sysstatus" "system" "systime" "sysuserfatal" "table" "task"
	 "template" "timeout" "title" "to" "trailer" "type" "underflow"
	 "underline" "units" "unlink" "until" "up" "update" "upshift"
	 "use" "using" "value" "vircoldim" "vircolof" "virlindim"
	 "virlinoff" "wait" "week" "weekday" "whenever" "while" "white"
	 "wincount" "window" "write" "year" "yellow" "zerodivide"
	 "zerofill") 'symbols) . font-lock-keyword-face))
  "Minimal highlighting expressions for rosi mode.")


;; (defvar rosi-mode-syntax-table
;;   (let ((st (make-syntax-table)))
;;     (modify-syntax-entry ?_ "w" st)
;;     st)
;;   "Syntax table for rosi-mode.")

;;; Code

(defun rosi-which-function ()
  "Return the name of the function point is in."
  (save-excursion
    (end-of-line) ;; make sure we get the complete name
    (if (re-search-backward "^ *PROCEDURE\\(.*\\)" 0 t)
	(match-string-no-properties 1))))

(defvar rosi-mode-syntax-table
  (let ((table (make-syntax-table)))
    ;; Give punctuation syntax to ASCII that normally has symbol
    ;; syntax or has word syntax and isn't a letter.
    (modify-syntax-entry ?$ "." table)
    ;; (modify-syntax-entry ?% "." table)
    ;; exceptions
    ;; (modify-syntax-entry ?# "<" table)
    (modify-syntax-entry ?\/ ". 14bn" table)
    (modify-syntax-entry ?* ". 23bn" table)

    ;; (modify-syntax-entry ?\/ ". 12b" table)
    ;; (modify-syntax-entry ?\n "> b" table)
    ;; (modify-syntax-entry ?# "< b" table)
    ;; (modify-syntax-entry ?\n ">" table)
    ;; (modify-syntax-entry ?' "\"" table)

    ;; (modify-syntax-entry ?` "$" table)
    table)
  "Syntax table for RosiSQL files.")

(define-derived-mode rosi-mode prog-mode "Rosi"
  "Major mode for editing rosi sql files."
  (set-syntax-table rosi-mode-syntax-table)
  (setq-local which-func-functions '(rosi-which-function))
  (setq-local imenu-generic-expression
	      '(("SQL"       "\\(^.*FROM SQL[^❄]*?END\\)" 1)
		("FIELD"     "^[\t ]*FIELD\\(.*\\)" 1)
		("PROCEDURE" "^[\t ]*PROCEDURE\\(.*\\)" 1)))
  (set (make-local-variable 'font-lock-defaults) '(rosi-font-lock-keywords nil t)))

(add-to-list 'which-func-modes 'rosi-mode)

(provide 'rosi)
;;; rosi.el ends here
