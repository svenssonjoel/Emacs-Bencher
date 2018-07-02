
;; Joel Svensson 2018

(require 'seq)
(require 'cl)


;; Benchmark struct
(cl-defstruct benchmark name executable varying tags)

;; CSV data container. Two lists, one for default columns
;; and one for user specified data tags 
(cl-defstruct csv-data default tags)

;; Default csv header
(defconst default-csv-header '("Name")) 

(defun read-lines (filePath)
  "Return a list of lines of a file at filePath."
  (with-temp-buffer
    (insert-file-contents filePath)
    (split-string (buffer-string) "\n" t)))

(defun insert-bm (str)
  "Insert Emacs Bencher text into the benchmark output buffer"
  (insert (concat "[EMACS BENCHER] " str)))


; From the web (https://www.emacswiki.org/emacs/ElispCookbook#toc6)
(defun chomp (str)
  "Chomp leading and tailing whitespace from STR."
  (while (string-match "\\`\n+\\|^\\s-+\\|\\s-+$\\|\n+\\'"
		       str)
    (setq str (replace-match "" t t str)))
  str)

(defun string-join (sl delim)
  (mapconcat 'identity sl delim))

(defun string-substitute (what with in)
  (with-temp-buffer
    (insert in)
    (beginning-of-buffer)
    (while (search-forward what nil t)
      (replace-match with nil t))
    (buffer-string)))


(defun read-expressions-from-string (str)
  "Read a number of (all) expressions from a string."
  (let ((fun))
    (setq fun
	  (lambda (loc)
	    (condition-case err
		(let ((res (read-from-string str loc)))
		  (let ((e (car res))
			(n (cdr res)))
		    (cons e (funcall fun n))))
	      (end-of-file '()))))
    (funcall fun 0)))


			      
  
  



;; TODO: Break up into small functions.
;;       Just dispatch from here. 
;; TODO: Add error checking (what if there is no car (cdr x) ?? (faulty .bench file)
;; TODO: Can read out several expressions (read-from-string returns both parsed elisp and number of characters consumed).

(defun parse-benchmark (bench l)
  "Parse a single benchmark until a line with %% is found"
  (if (not l)
      ()
    (if (not (string-prefix-p "%%" (car l)))
	(let ((ps (split-string (car l) ":")))
	  (let ((key (car ps))
		(value (chomp (car (cdr ps))))) ; for simple cases 
	    (cond ((string= key "name") (setf (benchmark-name bench) value))
		  ((string= key "tag")
		   (let* ((value-words (split-string value " "))
			  (tag-name (car value-words))
			  (tag-type (car (cdr value-words))))
		     (setf (benchmark-tags bench)
			   (cons (list tag-name tag-type)
				 (benchmark-tags bench)))))
		  ((string= key "varying")
		   (let* ((value-words (split-string value " "))
			  (value-variable (car value-words))
			  (value-body (string-join (cdr value-words) " ")))
		     (setf (benchmark-varying bench)
			   (cons (list value-variable value-body)
				 (benchmark-varying bench)))))
		  ((string= key "executable")
		   (setf (benchmark-executable bench) value)))
	    (parse-benchmark bench (cdr l))))
      l)
    )
  )

(defun parse-benchmarks (ls)
  "parse strings into benchmarks"
  (if ls
      (if (string-prefix-p "%%" (car ls))
	  (if (cdr ls)
	      (let ((bench (make-benchmark)))
		(let ((rest (parse-benchmark bench (cdr ls))))
		  (cons bench (parse-benchmarks rest))))
	    () ; Done!
	    )
	(message "Error parsing benchmark"))
    ())
  )


(defun do-substitutions (strs vars values)
  "Substitues variables from list vars with values from list values in list of strings"
  (if (not vars) strs
    (let* ((var (concat "%" (car vars)))
	   (val (car values)))
      (let ((strs-new (mapcar (lambda (x) (string-substitute var val x)) strs)))
	(do-substitutions strs-new (cdr vars) (cdr values))))))
 

(defun run-benchmark (bench)
  "Run a single benchmark"
  (let ((work-dir default-directory)
	(prev-buf (current-buffer)))
    (let (( buf (get-buffer-create (benchmark-name bench))))
      (set-buffer buf)
      ;(insert "Running benchmark\n")
      (let ((default-directory work-dir))
	(insert-bm (format "Running benchmark: %s\n" (benchmark-name bench)))
	(if (not (benchmark-varying bench))
	    (make-process  :name (benchmark-name bench)
			   :command (benchmark-executable bench)
			   :buffer (benchmark-name bench))
	  (let* ((varying-exps
		  (mapcar (lambda (x)
			    (mapcar 'number-to-string x))
			  (mapcar (lambda (x)
				    (car (read-from-string (car (cdr x))))) 
			  (benchmark-varying bench))))
		 (varying-vars
		  (mapcar 'car (benchmark-varying bench)))
		 (varying-selections
		  (all-selections varying-exps)))
	    (dolist (elt varying-selections ())
	      ;; TODO: Keep hacking here. 
	      (let* ((exec-cmd-orig (benchmark-executable bench))
		     (exec-cmd-str (car (do-substitutions (list exec-cmd-orig)
							  varying-vars elt)))
		     (exe-args-exprs (read-expressions-from-string exec-cmd-str))
		     (exe-args-evaled (mapcar 'eval (cdr exe-args-exprs)))
		     (exec-args (mapcar 'number-to-string exe-args-evaled))
		     (exec-sym (symbol-name (car exe-args-exprs)))
		     (exec-full (if (string-prefix-p "./" exec-sym)
				    (expand-file-name exec-sym)
				  (exec-sym)))
		     (exec-cmd (cons exec-full exec-args)))
		;(insert-bm (format "%s\n" exe-args-exprs))
		;(insert-bm (format "%s\n" exe-args-evaled))
		;(insert-bm (format "%s\n" exec-args))
		(insert-bm (format "Launching executable: %s\n" (car exec-cmd)))
		(make-process :name (benchmark-name bench)
			      :command exec-cmd
			      :buffer (benchmark-name bench))))))
	      
	)
      (set-buffer prev-buf)
      ))
  )


(directory-files ".")


(defun run-benchmarks (benches)
  "Run all benchmarks in a list"
  (if benches
      (progn
	(run-benchmark (car benches))
	(run-benchmarks (cdr benches))
	)
    ())
  )

;; (split-string (replace-regexp-in-string "[ ]+" "" "apa: bepa") ":")


(defun a (arg)
  "testing"
  (run-benchmarks (parse-benchmarks (read-lines "./test.bench"))))

(defun b (arg)
  "testing"
  (parse-benchmarks (read-lines "./test.bench")))


(defun all-selections (l)
  "generate all possible selections of one element per list from list of list"
  (if (not l) '(()) ; nothing to select from
    (let* ((a (car l))
	   (b (all-selections (cdr l))))
      (let (fun)
	(setf fun (lambda (l ls) (if (not l) ()
				   (let ((a (car l))
					 (b (cdr l)))
				     (append (mapcar (lambda (x) (cons a x)) ls)
					     (funcall fun b ls))))))
      
	(funcall fun a b)))))

	
	
  
