
;; Joel Svensson 2018

(require 'seq)
(require 'cl)


(cl-defstruct benchmark name executable varying)

(defun read-lines (filePath)
  "Return a list of lines of a file at filePath."
  (with-temp-buffer
    (insert-file-contents filePath)
    (split-string (buffer-string) "\n" t)))


; From the web (https://www.emacswiki.org/emacs/ElispCookbook#toc6)
(defun chomp (str)
  "Chomp leading and tailing whitespace from STR."
  (while (string-match "\\`\n+\\|^\\s-+\\|\\s-+$\\|\n+\\'"
		       str)
    (setq str (replace-match "" t t str)))
  str)


(defun parse-benchmark (bench l)
  "Parse a single benchmark until a line with %% is found"
  (if (not l)
      ()
    (if (not (string-prefix-p "%%" (car l)))
	(let ((ps (split-string (car l) ":")))
	  (let ((key (car ps))
		(value (chomp (car (cdr ps))))) ; for simple cases 
	    (cond ((string= key "name") (setf (benchmark-name bench) value))
		  ((string= key "executable")
		   (let ((tmp (split-string value " ")))
		     (let ((exe-args (if (string-prefix-p "./" (car tmp))
					 (cons (expand-file-name (car tmp)) (cdr tmp))
				       tmp)))
		       (setf (benchmark-executable bench) exe-args)))))
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

(defun run-benchmark (bench)
  "Run a single benchmark"
  (let ((work-dir default-directory)
	(prev-buf (current-buffer)))
    (let (( buf (get-buffer-create (benchmark-name bench))))
      (set-buffer buf)
      (insert "Running benchmark\n")
      (let ((default-directory work-dir))
	(message "Running benchmark: %s" (benchmark-name bench))
	(make-process  :name (benchmark-name bench)
		       :command (benchmark-executable bench)
		       :buffer (benchmark-name bench))
	(set-buffer prev-buf))
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
  (run-benchmarks (parse-benchmarks (read-lines "./test.txt"))))


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

	
	
  
