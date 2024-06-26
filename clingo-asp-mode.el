;;; clingo-asp-mode.el ---- A major mode for editing clingo ASP files. -*- lexical-binding: t -*-

;;; Commentary:

;; no commentary for the moment

;;; Code:
(defgroup clingo-asp nil
  "Commands used by `clingo-asp-mode'."
  :group 'clingo-asp)

;; general defcustoms
(defcustom clingo-asp-mode-version "0.0.1"
  "Version of `clingo-asp-mode'."
  :type 'string
  :group 'clingo-asp)

(defcustom clingo-asp-clingo-executable (executable-find "clingo")
  "Path to clingo binary used for execution."
  :type 'string
  :group 'clingo-asp)


(defcustom clingo-asp-gringo-executable (executable-find "gringo")
  "Path to gringo binary used for execution."
  :type 'string
  :group 'clingo-asp)

(defcustom clingo-asp-reuse-clingo-buffer t
  "If a buffer from a clingo call exists, clear and reuse on a new solve."
  :type 'boolean
  :group 'clingo-asp)

(defcustom clingo-asp-reuse-gringo-buffer t
  "If a buffer from a gringo call exists, clear and reuse on a new solve."
  :type 'boolean
  :group 'clingo-asp)

(defcustom clingo-asp-indentation 2
  "Level of indentation."
  :type 'integer
  :group 'clingo-asp)

(defcustom clingo-asp-default-arguments-choice "no arguments"
  "Default choice of arguments."
  :type 'string
  :group 'clingo-asp)
;; general defcustoms end

;; arguments defaults
(defcustom clingo-asp-default-clingo-outf 0
  "Default value of outf when calling clingo."
  :type 'integer
  :group 'clingo-asp)

(defcustom clingo-asp-default-clingo-models 5
  "Default value of outf when calling clingo."
  :type 'integer
  :group 'clingo-asp)

(defcustom clingo-asp-default-clingo-help 2
  "Default value of outf when calling clingo."
  :type 'integer
  :group 'clingo-asp)

(defcustom clingo-asp-default-clingo-warn "all"
  "Default warnings when calling clingo."
  :type 'string
  :group 'clingo-asp)


(defvar clingo-asp-default-plists
  '((:default clingo-asp-default-clingo-outf
     :match ("--outf")
     :format "--outf=%s")
    (:default clingo-asp-default-clingo-models
     :match ("--models" "-n")
     :format "--models=%s")
    (:default clingo-asp-default-clingo-warn
     :match ("--warn" "-W")
     :format "--warn=%s")))
;; argument defaults end

;;
(defcustom clingo-asp-mode-hook nil
  "Hooks called when `clingo-asp-mode' is enabled."
  :type 'hook
  :group 'clingo-asp)
;;


;; exit code start
(defconst clingo-asp-exit-codes
  '((128 "E_NO_RUN" "Search not started because of syntax or command line error.")
    (65 "E_ERROR" "Run was interrupted by internal error.")
    (33 "E_MEMORY" "Run was interrupted by out of memory exception.")
    (20 "E_EXHAUST" "Search-space was completely examined.")
    (10 "E_SAT" "At least one model was found.")
    (1 "E_INTERRUPT" "Run was interrupted.")
    (0 "E_UNKNOWN" "Satisfiablity of problem not known; search not started.")))


(defun clingo-asp-decode-exit (code)
  "Decode CODE into a list of base codes."
  ;; see https://github.com/potassco/clasp/issues/42#issuecomment-459981038%3E
  (let ((decomposed '()))
    (defun process-code (number)
      (setq code (- code number))
      (setq decomposed (cons number decomposed)))
    (if (= code 0) (process-code 0))
    (if (>= code 128) (process-code 128))
    (if (>= code 65) (process-code 65))
    (if (>= code 33) (process-code 33))
    (if (>= code 20) (process-code 20))
    (if (>= code 10) (process-code 10))
    (if (>= code 1) (process-code 1))
    (clingo-asp-codes-to-string decomposed)))


(defun clingo-asp-codes-to-string (codes)
  "Return a summary string of CODES."
  (let* ((code-sym (string-join (mapcar (lambda (x) (cadr (assoc x clingo-asp-exit-codes))) codes)" + "))
         (code-exp (string-join (mapcar (lambda (x) (caddr (assoc x clingo-asp-exit-codes))) codes) " ")))
    (format "%s (%s)" code-sym code-exp)))


(defun clingo-asp-clingo-process-exit (process-name process-buffer)
  "Use with `set-process-sentinel' to perform actions after PROCESS-NAME exits.
PROCESS-BUFFER is the buffer for the process output."
  (lambda (process event)
    (if (and (> (length event) 27)
             (equal (substring event 0 27) "exited abnormally with code"))
        (princ (format "Process: %s exited with: %s" process (clingo-asp-decode-exit (string-to-number (car (last (split-string event))))))))
    (let ((the-process-buffer (if clingo-asp-reuse-clingo-buffer
                                  process-buffer
                                (get-buffer process-name))))
      (with-current-buffer the-process-buffer
        (goto-char (point-max))
        (insert "---\n")
        ))))


(defun clingo-asp-gringo-process-exit (process-name)
  "Use with `set-process-sentinel' to perform actions after PROCESS-NAME exits."
  (lambda (process event)
    (let ((process-buffer (get-buffer process-name)))
      (if (not (string-equal "finished" event))
          (message (format "Process: %s: %s" process event)))
      (with-current-buffer process-buffer
        (special-mode)
        (goto-char (point-min))))))
;; exit code end


;; choosing arguments
(defvar clingo-asp-arguments-list
  '((:name "no arguments"
     :evaluate nil
     :commands ()
     :help "no arguments")
    (:name "all models"
     :evaluate nil
     :commands ("--models=0")
     :help "")
    (:name "all subset minimal models"
     :evaluate nil
     :commands ("--models=0" "--enum-mode=domRec" "--heuristic=Domain" "--dom-mod=5,16")
     :help "")
    (:name "custom"
     :evaluate t
     :commands (string-split (read-string "Commands:"))
     :help "enter commands in a prompt")
    (:name "n models"
     :evaluate t
     :commands (string-split (format "--models=%s" (read-string "Number of models:")))
     :help "--models=(prompt: n)")
    (:name "optimal model"
     :evaluate nil
     :commands ("--opt-mode=opt")
     :help "")
    (:name "enumerate optimal models"
     :evaluate nil
     :commands ("--opt-mode=optN")
     :help "")
    (:name "ignore optimize statements"
     :evaluate nil
     :commands ("--opt-mode=ignore")
     :help "")
    (:name "help"
     :evaluate t
     :commands (string-split (format "--help=%s" (eval clingo-asp-default-clingo-help)))
     :help "--help")))


(defcustom clingo-asp-arguments-help-separator "  "
  "String used to separate argument name from help.
Used when interactively choosing arguments."
  :type 'string
  :group 'clingo-asp)


(defun clingo-asp-annotate-command (command)
  "Get annotation for COMMAND.
Used in `clingo-asp-arguments-query'."
  (concat clingo-asp-arguments-help-separator (clingo-asp-get-args-or-help clingo-asp-arguments-list command)))


(defun clingo-asp-get-args-or-help (command-list command)
  "Helper for `clingo-asp-annotate-command'.
If COMMAND-LIST contains plists with :name, :commands, and :help,
 reutrn :help if non-empty and otherwise :commands when :name is COMMAND."
  (if (eq command-list '())
      ""
    (if (string-equal command (plist-get (car command-list) ':name))
        (let ((help-string (plist-get (car command-list) ':help)))
          (if (string-equal help-string "")
              (string-join (plist-get (car command-list) ':commands) " ")
            help-string))
      (clingo-asp-get-args-or-help (cdr command-list) command))))


(defun clingo-asp-get-default-plist (default plists)
  "From PLISTS get plist whose :default matches DEFAULT."
  (cond ((not plists) '())
        ((eq default (plist-get (car plists) ':default)) (car plists))
        (t (clingo-asp-get-default-plist default (car plists)))))


(defun clingo-asp-default-to-add (default-arg-plist arg-string)
  "Formatted argument using DEFAULT-ARG-PLIST if arg is not in ARG-STRING.
Otherwise, the empty string."
  (if (string-match-p (regexp-opt (plist-get default-arg-plist ':match) "\\(?:[[:blank:]]") arg-string)
      nil
    (string-trim (format (plist-get default-arg-plist ':format) (eval (plist-get default-arg-plist ':default))))))

(defun clingo-asp-add-defaults (arg-list)
  "Append viable defaults to ARG-LIST."
  (let* ((nice-arg-string (concat " " (string-join arg-list " ")))) ;; leading " " to avoid complex regex
    (remq nil (append arg-list (mapcar (lambda (x) (clingo-asp-default-to-add x nice-arg-string)) clingo-asp-default-plists)))))
;; choosing arguments end


;; local-storage
(defvar clingo-asp-clingo-buffer-list '())

;; calling programs

;; ;; helpers

;; todo: update list to have :programs list, pass clingo/gringo etc. as an argument and filter commands based on this.
(defun clingo-asp-arguments-query ()
  "Query user for arguments to pass to clingo."
  (let* ((default clingo-asp-default-arguments-choice)
         (completion-ignore-case t)
         (completion-extra-properties '(:annotation-function clingo-asp-annotate-command))
         (command-plist-list (mapcar (lambda (x) (cons (plist-get x ':name) x)) clingo-asp-arguments-list))
         (answer (completing-read
                  (concat "Command (default " default "): ")
                  command-plist-list nil t
                  nil nil default)))
    (let* ((the-plist (cdr (assoc answer command-plist-list)))
           (the-commands (plist-get the-plist ':commands))
           (eval-required (plist-get the-plist ':evaluate)))
      (if eval-required
          (eval the-commands)
        the-commands))))

(defun clingo-asp-interactively-get-file-list (file)
  "A list of interactively chosen FILEs.
Choosing anything other than an existing file ends choice.
E.g. if `done' is not a file choose `done' to return the list."
  (interactive "F")
  (if (file-exists-p file)
      (cons file (call-interactively #'clingo-asp-interactively-get-file-list))
    (list )))
;; ;; helpers end


(defun clingo-asp-call-clingo (files args)
  "Run clingo on FILES with ARGS as a new process with it's own buffer."
  (let* ((args-files (append (clingo-asp-add-defaults args) (mapcar #'file-truename files)))
         (clingo-process (generate-new-buffer-name "*clingo*"))
         (clingo-buffer (if clingo-asp-reuse-clingo-buffer
                            (if (get-buffer "*cligo*")
                                (get-buffer "*cligo*")
                              (let ((new-buffer (get-buffer-create "*clingo*")))
                                (princ "a new clingo buffer")
                                (with-current-buffer new-buffer
                                  (insert "\n")
                                  (special-mode)
                                  (setq buffer-read-only nil))
                                new-buffer))
                          (get-buffer-create clingo-process))))
    (apply #'make-process
           (list :name clingo-process
                 :buffer clingo-buffer
                 :command (cons clingo-asp-clingo-executable args-files)
                 :sentinel (clingo-asp-clingo-process-exit clingo-process clingo-buffer)))
    (if (not clingo-asp-reuse-clingo-buffer)
        (progn
          (setq clingo-asp-clingo-buffer-list (cons clingo-buffer clingo-asp-clingo-buffer-list))
          (with-current-buffer clingo-buffer
            (add-hook 'kill-buffer-hook (clingo-asp-kill-clingo-buffer-hook clingo-buffer)))))
    (display-buffer clingo-buffer)))


(defun clingo-asp-kill-clingo-buffer-hook (clingo-buffer)
  "Function to be run when a created CLINGO-BUFFER is killed."
  (lambda ()
    (progn
      (setq clingo-asp-clingo-buffer-list (remove clingo-buffer clingo-asp-clingo-buffer-list)))))


(defun clingo-asp-kill-all-clingo-buffers ()
  "Kill any live clingo buffers."
  (interactive)
  (if clingo-asp-clingo-buffer-list
      (progn
        (kill-buffer (car clingo-asp-clingo-buffer-list))
        (clingo-asp-kill-all-clingo-buffers))))


(defun clingo-asp-call-clingo-on-buffer ()
  "Call `clingo-asp-call-clingo-choice' on the current buffer contents."
  (interactive)
  (let ((temp-file (make-temp-file "clingo-buffer" nil ".lp" nil)))
    (write-region (point-min) (point-max) temp-file t)
    (clingo-asp-call-clingo-choice (list temp-file))))
  ;; (let ((this-file (buffer-file-name)))
  ;;   (clingo-asp-call-clingo-choice (list this-file))))


(defun clingo-asp-call-clingo-on-current-region (start end)
  "Run clingo on the region from START to END."
  (interactive "r")
  (let ((temp-file (make-temp-file "clingo-region" nil ".lp" nil)))
    (write-region start end temp-file t)
    (clingo-asp-call-clingo-choice (list temp-file))))


(defun clingo-asp-call-clingo-choice (files)
  "Call `clingo-asp-call-clingo' on FILES with chosen arguments."
  (clingo-asp-call-clingo files (clingo-asp-arguments-query)))


(defun clingo-asp-call-clingo-file-choice (file)
  "Call `clingo-asp-call-clingo-choice' on interactively chosen FILE."
  (interactive "f")
  (clingo-asp-call-clingo-choice (list file)))


(defun clingo-asp-call-clingo-files-choice ()
  "Call `clingo-asp-call-clingo-choice' on interactively chosen files."
  (interactive)
  (clingo-asp-call-clingo-choice (call-interactively #'clingo-asp-interactively-get-file-list)))


(defun clingo-asp-call-gringo (files args)
  "Run gringo on FILES with ARGS as a new process with it's own buffer."
  (let* ((args-files (append args (mapcar #'file-truename files)))
         (gringo-process (generate-new-buffer-name "*gringo*"))
         (gringo-buffer (if clingo-asp-reuse-gringo-buffer
                            (get-buffer-create "*gringo*")
                          (get-buffer-create gringo-process))))
    (apply #'make-process
           (list :name gringo-process
                 :buffer gringo-buffer
                 :command (cons clingo-asp-gringo-executable args-files)
                 :sentinel (clingo-asp-gringo-process-exit gringo-process)))
    (pop-to-buffer gringo-buffer)))

(defun clingo-asp-call-gringo-on-buffer ()
  "Call `clingo-asp-call-gringo-choice' on the current buffer contents."
  (interactive)
  (let ((temp-file (make-temp-file "clingo-buffer" nil ".lp" nil)))
    (write-region (point-min) (point-max) temp-file t)
    (clingo-asp-call-clingo-choice (list temp-file))))


(defun clingo-asp-call-gringo-on-current-region (start end)
  "Run clingo on the region from START to END."
  (interactive "r")
  (let ((temp-file (make-temp-file "clingo-region" nil ".lp" nil)))
    (write-region start end temp-file t)
    (clingo-asp-call-gringo-choice (list temp-file))))


(defun clingo-asp-call-gringo-choice (files)
  "Call `clingo-asp-call-gringo' on FILES with chosen arguments."
  (clingo-asp-call-gringo files (clingo-asp-arguments-query)))


(defun clingo-asp-call-gringo-file-choice (file)
  "Call `clingo-asp-call-gringo-choice' on interactively chosen FILE."
  (interactive "f")
  (clingo-asp-call-gringo-choice (list file)))


(defun clingo-asp-call-gringo-files-choice ()
  "Call `clingo-asp-call-gringo-choice' on interactively chosen files."
  (interactive)
  (clingo-asp-call-gringo-choice (call-interactively #'clingo-asp-interactively-get-file-list)))

;; calling clingo end



;; font-lock
(defcustom clingo-asp-font-lock-keywords
  '((":-" . 'font-lock-punctuation-face)
    ("\\(?:not\\|-[A-Za-z0-9_']\\)" . 'font-lock-negation-char-face)
    ("0x[0-9A-Fa-f]+" . 'font-lock-number-face) ;; hexadeciamal
    ("0o[1-7]+" . 'font-lock-number-face) ;; octal
    ("0b[0-1]+" . 'font-lock-number-face) ;; binary
    ("0\\|\\(?:[1-9][0-9]*\\)" . 'font-lock-number-face) ;; deciamal
    ("[_']*[A-Z][A-Za-z0-9_']*" . 'font-lock-variable-use-face) ;; variable
    ("_*[a-z][A-Za-z0-9_']*" . 'font-lock-constant-face) ;; identifier/constant
    )
"Font definitions for `clingo-asp'."
:type '(repeat ('string 'symbol)))
;; font-lock end


;; syntax table
(defvar clingo-asp-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?. "." table)
    (modify-syntax-entry ?\" "\"" table)
    (modify-syntax-entry ?% "<" table)
    (modify-syntax-entry ?\n ">" table)
    (modify-syntax-entry ?' "w" table)
    (modify-syntax-entry ?_ "w" table)
    (modify-syntax-entry ?# "'" table)
    table))
;; syntax table end


;; keymap
(defvar clingo-asp-mode-map (make-sparse-keymap)  "Keymap for `clingo-asp-mode'.")

(define-key clingo-asp-mode-map (kbd "C-c C-c b") #'clingo-asp-call-clingo-on-buffer)
(define-key clingo-asp-mode-map (kbd "C-c C-c r") #'clingo-asp-call-clingo-on-current-region)
(define-key clingo-asp-mode-map (kbd "C-c C-c f") #'clingo-asp-call-clingo-file-choice)
(define-key clingo-asp-mode-map (kbd "C-c C-c F") #'clingo-asp-call-clingo-files-choice)
(define-key clingo-asp-mode-map (kbd "C-c C-c K") #'clingo-asp-kill-all-clingo-buffers)

(define-key clingo-asp-mode-map (kbd "C-c C-g b") #'clingo-asp-call-gringo-on-buffer)
(define-key clingo-asp-mode-map (kbd "C-c C-g r") #'clingo-asp-call-gringo-on-current-region)
(define-key clingo-asp-mode-map (kbd "C-c C-g f") #'clingo-asp-call-gringo-file-choice)
(define-key clingo-asp-mode-map (kbd "C-c C-g F") #'clingo-asp-call-gringo-files-choice)

;; keymap end

;;; define clingo-asp-mode
;;;###autoload
(define-derived-mode clingo-asp-mode prog-mode "clingo-asp"
  (kill-all-local-variables)
  (setq-local major-mode 'clingo-asp-mode)
  (setq-local mode-name "Clingo ASP")
  (setq-local font-lock-defaults '(clingo-asp-font-lock-keywords))
  (set-syntax-table clingo-asp-syntax-table)
  (use-local-map clingo-asp-mode-map)
  (setq-local comment-start "%")
  (setq-local comment-end "")
  (setq-local tab-width clingo-asp-indentation)
  (run-mode-hooks))

(provide 'clingo-asp-mode)
;;; clingo-asp-mode.el ends here
