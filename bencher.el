
;; This file is part of Emacs-Bencher.

;; Emacs-Bencher is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; Emacs-Bencher is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with Emacs-Bencher.  If not, see <https://www.gnu.org/licenses/>.

;; Joel Svensson 2018

(require 'seq)
(require 'cl)


;; ------------------------------------------------------------
;; Version

(defconst emacs-bencher-version '(0 0 0))
(defun emacs-bencher-version-string ()
  "Returns version string"
  (let ((a (car emacs-bencher-version))
	(b (car (cdr emacs-bencher-version)))
	(c (car (cdr (cdr emacs-bencher-version)))))
    (format "%s.%s.%s" a b c)))

;; ------------------------------------------------------------
;; data structures 

;; Benchmark struct
(cl-defstruct benchmark name executable varying tags)

;; Benchmark "run-unit"
(cl-defstruct benchmark-run-unit name exec-cmd tags)

;; CSV data container. Two lists, one for default columns
;; and one for user specified data tags 
(cl-defstruct csv-format default tags)

;; Default csv header
(defconst default-csv-header '("Name")) 

;; ------------------------------------------------------------
;; State of Emacs-Bencher (Is it possible to prohibit changes to
;; these from the outside?)

(defvar emacs-bencher-running-benchmark nil)
(defvar emacs-bencher-scheduled-benchmarks-list ()) ; benches to run
(defvar emacs-bencher-benchmark-run-timer nil)

(defvar emacs-bencher-run-unit-sentinel nil)

(defvar csv-data ()) ; var to collect csv data into (key value) pair list
					

;debug
(setq emacs-bencher-scheduled-benchmarks-list ())
(setq emacs-bencher-running-benchmark nil)
;(cancel-timer (car timer-list))

;; ------------------------------------------------------------
;; Emacs-Bencher message buffer
(setq emacs-bencher-messages (get-buffer-create "*Bencher-messages*"))
(let ((prev-buf (current-buffer)))
  (set-buffer emacs-bencher-messages)
  (setq buffer-read-only t)
  (set-buffer prev-buf))

(defun message-eb (string)
  "Add a message string to the *Bencher-messages* buffer"
  (let ((prev-buf (current-buffer)))
    (set-buffer emacs-bencher-messages)
    (setq buffer-read-only nil)
    (goto-char (point-max))
    (insert (concat string "\n"))
    (setq buffer-read-only t)

    ;; 
    (set-window-point
     (get-buffer-window emacs-bencher-messages 'visible)
     (point-max))
    
    
    (set-buffer prev-buf)))
 

(defun clear-bencher-messages ()
  "Clear the *Bencher-messages* buffer"
  (let ((prev-buf (current-buffer)))
    (set-buffer emacs-bencher-messages)
    (setq buffer-read-only nil)
    (setf (buffer-string) "")
    (setq buffer-read-only t)
    (set-buffer prev-buf)))

;; ------------------------------------------------------------
;; CODE!

(defun read-lines (filePath)
  "Return a list of lines of a file at filePath."
  (with-temp-buffer
    (insert-file-contents filePath)
    (split-string (buffer-string) "\n" t)))

(defun insert-eb (str)
  "Insert Emacs Bencher text into the benchmark output buffer"
  (insert (concat "[EMACS BENCHER] " str)))

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


;; TODO: Break up into small functions. Just dispatch from here. 
;; TODO: Add error checking (what if there is no car (cdr x) ?? (faulty .bench file)

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
		   ; Bit ugly that the string is split at spaces and then recombined..
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
	(message-eb "Error parsing benchmark"))
    ())
  )


(defun do-substitutions (strs vars values)
  "Substitues variables from list vars with values from list values in list of strings"
  (if (not vars) strs
    (let* ((var (car vars)) ; (concat "%" (car vars)))
	   (val (format " %s " (car values))))
      (let ((strs-new (mapcar (lambda (x)
				(if (string= var x)
				    var
				  (if (string-match
				       (concat "[\s \t]+"
					       (concat var "[\s \t]+")) x 0)
				      (replace-match val nil nil x)
				    (if (string-match
					 (concat "[\s \t]*"
						 (concat var "\\'")) x 0)
					(replace-match val nil nil x)
				      x)))
				  ) strs )))
	(do-substitutions strs-new (cdr vars) (cdr values))))))
 
(defun tag-parsing-filter (name tags proc string)
  "Filter process output and parse out tag data"
  (message-eb "running filter")
  
  (when (buffer-live-p (process-buffer proc))
    (with-current-buffer (process-buffer proc)
      (let ((moving (= (point) (process-mark proc))))
	(save-excursion
	  ;; Insert the text, advancing the process marker.
	  (goto-char (process-mark proc))
	  (insert string)
	  (set-marker (process-mark proc) (point)))
	(if moving (goto-char (process-mark proc))))))
  ;; Parse tags and put in csv data
  (let* ((tmp-lines (split-string string "\n" t))
	 (output-lines (mapcar 'chomp tmp-lines))) ; dont think needed
    (add-tag-values output-lines tags))
  )

;; Dont think the types are needed on the tags... remove it 
(defun add-tag-values (data tags)
  (if data
      (progn 
	(let* ((key-val (split-string (car data) ":"))
	       (key (car key-val))
	       (val (car (cdr key-val)))
	       (key-tag-assoc (assoc key tags)))
	  (if (= (length key-tag-assoc) 2)
	      (progn
		(message-eb (format "starting a tag parse %s" key-val))
		(cond
		 ((string= (car (cdr key-tag-assoc)) "double")
		  (setq csv-data (cons (cons key val) csv-data)))
		 ((string= (car (cdr key-tag-assoc)) "int")
		  (setq csv-data (cons (cons key val) csv-data)))
				
		 (t ())))
	    ()))
	(add-tag-values (cdr data) tags))))

  
  
(defun run-benchmarks ()
  "Process enqueued benchmarks"
  (message-eb (format "There are %s benchmarks to process"
	   (length emacs-bencher-scheduled-benchmarks-list)))
  (if (not emacs-bencher-scheduled-benchmarks-list)
      (cancel-timer emacs-bencher-benchmark-run-timer) ; Turn off recurring timer
    (if emacs-bencher-running-benchmark
	() ; Benchmark already in progress just return
      (progn
				
	(setq emacs-bencher-running-benchmark t)  ; A new bench run is starting
	
	(let* ((bench (car emacs-bencher-scheduled-benchmarks-list))
	       (prev-buf (current-buffer))
	       (buf (get-buffer-create (benchmark-run-unit-name bench))))
	  (setq emacs-bencher-scheduled-benchmarks-list
		(cdr emacs-bencher-scheduled-benchmarks-list))

	  (setq csv-data ()) ; prepare to collect new csv data				
	  (setq csv-data (cons (cons "Name"  (benchmark-run-unit-name bench)) csv-data)) ; dotted pair

	  
	  (set-buffer buf)
	  (message-eb (format "Running benchmark: %s" (benchmark-run-unit-name bench)))
	  (let ((proc (make-process :name (benchmark-run-unit-name bench)
	    			    :command (benchmark-run-unit-exec-cmd bench)
				    :buffer (benchmark-run-unit-name bench))))
	    (set-process-sentinel
	     proc
	     (lexical-let ((buf buf)) ;
	       (lambda (process signal)
		 (cond
		  ((equal signal "finished\n")
		   (progn
		     (message-eb (format "Benchmark finished! %s" buf))
		     (message-eb (format "collected data: %s" csv-data))
		     (setq emacs-bencher-running-benchmark nil)))))))
	    (set-process-filter
	     proc
	     (lexical-let ((bench bench))
	       (lambda (proc string)
		 (tag-parsing-filter
		  (benchmark-run-unit-name bench)
		  (benchmark-run-unit-tags bench)
		  proc
		  string)))))
	  )))))
  

(defun do-benchmarks (benches)
  "Enqueue all benchmarks and start the benchmark processing timer func"
  (if emacs-bencher-scheduled-benchmarks-list
      (message "Error: There are already scheduled benchmarks") ; An alternative is to just schedule more! 
    (if benches
	(progn
	  (enqueue-all-benches benches)
	  (if emacs-bencher-benchmark-run-timer
	      (cancel-timer emacs-bencher-benchmark-run-timer)
	    ())	
	  (setq emacs-bencher-benchmark-run-timer
		(run-at-time t 1 #'run-benchmarks))) 
      (message "Error: No benchmarks to run"))))

(defun enqueue-all-benches (benches)
  "Enqueue all benchmarks, add to scheduled benchmark list"
  (if benches
      (progn 
	(enqueue-benches (car benches))
	(enqueue-all-benches (cdr benches)))
    ()))

(defun enqueue-benches (bench)
  "Expand the varying space of the benchmark and enqueue each instance"
  ; Todo add a case for the bench without varying... 
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
      (let* ((exec-cmd-orig (benchmark-executable bench))
	     (exec-cmd-str (car (do-substitutions (list exec-cmd-orig)
						  varying-vars elt)))
	     (exe-args-exprs (read-expressions-from-string exec-cmd-str))
	     (exe-args-evaled (mapcar 'eval (cdr exe-args-exprs)))
	     (exec-args (mapcar 'number-to-string exe-args-evaled))
	     (exec-sym (symbol-name (car exe-args-exprs)))
	     (exec-full (if (string-prefix-p "./" exec-sym) ;; Expand to full filename (full path) 
			    (expand-file-name exec-sym)     ;; if a file in pwd is specified in the .bench file. 
			  (exec-sym)))                      ;; TODO: Alternatively figure out how to make make-process find executables in pwd. 
	     (exec-cmd (cons exec-full exec-args)))

	(let ((run-unit (make-benchmark-run-unit)))
	  (setf (benchmark-run-unit-name run-unit) (benchmark-name bench))
	  (setf (benchmark-run-unit-exec-cmd run-unit) exec-cmd)
	  (setf (benchmark-run-unit-tags run-unit) (benchmark-tags bench))
	  (setq emacs-bencher-scheduled-benchmarks-list
		(cons run-unit emacs-bencher-scheduled-benchmarks-list)))))))

; Debug
(defun a ()
  "testing"
  (run-benchmarks (parse-benchmarks (read-lines "./test.bench"))))

(defun b ()
  "testing"
  (parse-benchmarks (read-lines "./test.bench")))

(defun c ()
  "testing"
  (do-benchmarks (parse-benchmarks (read-lines "./test.bench"))))
  



