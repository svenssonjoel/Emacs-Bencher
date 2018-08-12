;; bencher.el --- Specify and run benchmarks and collect data 

;; Copyright (C) 2018 
;; Author: Joel Svensson <svenssonjoel@yahoo.se> 
;; Version: 0.0.0

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


(require 'seq)
(require 'cl)

;; ------------------------------------------------------------
;; Version
(defconst bencher-version "0.0.0")

;; ------------------------------------------------------------
;; data structures 

;; Benchmark struct
(cl-defstruct bencher-benchmark
  name 
  csv
  executable
  varying
  runs        ;; Number of times to run the benchmark with each varying setting.
  tags)

;; --- In progress --- 
;; Information needed to run an executable
(cl-defstruct bencher-executable name args)

;; Information needed to build and run a makefile based project
(cl-defstruct bencher-makefile-project make-args run-args)
;; -------------------

(cl-defstruct bencher-csv-format name args tags) ;; something like this ? 

;; Benchmark "run-unit"
(cl-defstruct bencher-run-unit
  name           ;; Benchmark name
  csv            ;; CSV output buffer name
  run-id         ;; connected to runs in bencher-benchmark struct.
  exec-args      ;; argument list (var, value) pairs 
  exec-cmd       ;; Binary to run 
  tags           ;; Tags of interest to this run-unit
  csv-header     ;; TODO: Come up with better names (This is only the non-tags part of the header) 
  csv-data       ;; Generated CSV data
  csv-data-tags) ;; Collected CSV data (from tags) list of key-value pairs

;; ------------------------------------------------------------
;; State of Emacs-Bencher 

(defvar bencher-running-benchmark nil)
(defvar bencher-scheduled-benchmarks-list '()) ; benches to run
(defvar bencher-benchmark-run-timer nil)

;; List of functions (that take a list of key-value pairs as input) to be run 
;; at the end of each benchmark. 
(defvar bencher-data-processing-plugin-list '()) 

(defconst bencher-time-cmd '("/usr/bin/time"
			     "-f"
			     "TimeReal: %e\nTimeUser: %U\nTimeSys: %S\nTimeNCS: %c"))
(defconst bencher-time-tags '("TimeReal" "TimeUser" "TimeSys" "TimeNCS"))

(defconst bencher-default-tags '())

(defconst bencher-messages-buffer-name "*Bencher-messages*")

;debug
(setq bencher-scheduled-benchmarks-list ())
(setq bencher-running-benchmark nil)

;; Example of a data processing plugin
;; (defun bencher-dummy-echo-to-messages (values)
;;   "dummy"
;;   (message (format "DUMMY: %s" values)))

;; (setq bencher-data-processing-plugin-list
;;       (cons #'bencher-dummy-echo-to-messages
;; 	    bencher-data-processing-plugin-list))

;; ------------------------------------------------------------
;; Emacs-Bencher message buffer
(setq bencher-messages (get-buffer-create bencher-messages-buffer-name))

(let ((prev-buf (current-buffer)))
  (set-buffer bencher-messages)
  (setq buffer-read-only t)
  (set-buffer prev-buf))

(defun bencher-message (string)
  "Add a message string to the *Bencher-messages* buffer"
  (let ((prev-buf (current-buffer)))
    (set-buffer bencher-messages)
    (setq buffer-read-only nil)
    (goto-char (point-max))
    (insert (concat string "\n"))
    (setq buffer-read-only t)

    ;; scroll the window
    (let ((buffer-win (get-buffer-window bencher-messages 'visible)))
      (if buffer-win
	  (set-window-point buffer-win
	   (point-max))))
    
    (set-buffer prev-buf)))
 

(defun bencher-clear-messages ()
  "Clear the *Bencher-messages* buffer"
  (let ((prev-buf (current-buffer)))
    (set-buffer bencher-messages)
    (setq buffer-read-only nil)
    (setf (buffer-string) "")
    (setq buffer-read-only t)
    (set-buffer prev-buf)))

;; ------------------------------------------------------------
;; CODE!

(defun bencher-read-lines (filePath)
  "Return a list of lines of a file at filePath."
  (with-temp-buffer
    (insert-file-contents filePath)
    (split-string (buffer-string) "\n" t)))

(defun bencher-insert (str)
  "Insert Emacs Bencher text into the benchmark output buffer"
  (insert (concat "[EMACS BENCHER] " str)))

(defun bencher-chomp (str)
  "Chomp leading and tailing whitespace from STR."
  (while (string-match "\\`\n+\\|^\\s-+\\|\\s-+$\\|\n+\\'"
		       str)
    (setq str (replace-match "" t t str)))
  str)

(defun bencher-string-join (sl delim)
  (mapconcat 'identity sl delim))

(defun bencher-string-substitute (what with in)
  (with-temp-buffer
    (insert in)
    (beginning-of-buffer)
    (while (search-forward what nil t)
      (replace-match with nil t))
    (buffer-string)))


(defun bencher-read-expressions-from-string (str)
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

(defun bencher-all-selections (l)
  "generate all possible selections of one element per list from a list of lists"
  (if (not l) '(()) ; nothing to select from
    (let* ((a (car l))
	   (b (bencher-all-selections (cdr l))))
      (let (fun)
	(setf fun (lambda (l ls) (if (not l) ()
				   (let ((a (car l))
					 (b (cdr l)))
				     (append (mapcar (lambda (x) (cons a x)) ls)
					     (funcall fun b ls))))))
      
	(funcall fun a b)))))


(defun bencher-parse-key-val (str)
  "Parse a colon separated key: value pair from text into a dotted pair. Return nil on error"
  (let ((tmp (split-string str ":")))
    (if (or (> (length tmp) 2)
	    (< (length tmp) 2))
	nil
      (cons (bencher-chomp (car tmp)) (bencher-chomp (car (cdr tmp)))))))

(defun bencher-parse-benchmark (bench l)
  "Parse a single benchmark until a line with %% is found"
  (if (not l)
      ()
    (if (not (string-prefix-p "%%" (car l)))
	(let ((ps (bencher-parse-key-val (car l))))
	  (let ((key (car ps))
		(value (cdr ps))) ; for simple cases 
	    (cond ((string= key "name") (setf (bencher-benchmark-name bench) value))
		  ((string= key "csv") (setf (bencher-benchmark-csv bench) value))
		  ((string= key "runs") (setf (bencher-benchmark-runs bench)
					      (string-to-number value)))
		  ((string= key "tags")
		   (let* ((tags-list-expr (read-from-string value))
			  (tags-list-strs
			   (eval (car tags-list-expr))))
		     (setf (bencher-benchmark-tags bench) tags-list-strs)))
    
		  ((string= key "varying")
		   ; Bit ugly that the string is split at spaces and then recombined..
		   (let* ((value-words (split-string value " "))
		   	  (value-variable (car value-words))
		    	  (value-body (bencher-string-join (cdr value-words) " ")))
		     (setf (bencher-benchmark-varying bench)
			   (cons (cons value-variable value-body)
				 (bencher-benchmark-varying bench)))))
		  ((string= key "executable")
		   (setf (bencher-benchmark-executable bench) value)))
	    (bencher-parse-benchmark bench (cdr l))))
      l)
    )
  )

(defun bencher-parse-benchmarks (ls)
  "parse strings into benchmarks"
  (if ls
      (if (string-prefix-p "%%" (car ls))
	  (if (cdr ls)
	      (let ((bench (make-bencher-benchmark)))
		(let ((rest (bencher-parse-benchmark bench (cdr ls))))
		  (cons bench (bencher-parse-benchmarks rest))))
	    () ; Done!
	    )
	(bencher-message "Error parsing benchmark"))
    ())
  )

(defun bencher-do-substitutions (strs var-vals)
  "Substitues variables with values in list of strings"
  (if (not var-vals) strs
    (let* ((var-val (car var-vals))
	   (var (car var-val)) 
	   (val (format " %s " (cdr var-val))))
      (let ((strs-new (mapcar (lambda (x)
				(if (string= var x)
				    var
				  (if (string-match
				       (concat "[\s \t]+" (concat var "[\s \t]+"))
				       x 0)
				      (replace-match val nil nil x)
				    (if (string-match
					 (concat "[\s \t]*" (concat var "\\'"))
					 x 0)
					(replace-match val nil nil x)
				      x))))
			      strs)))
	(bencher-do-substitutions strs-new (cdr var-vals))))))

(defun bencher-parse-buffer-for-tags (buf tags)
  "Look for occurances of tags in buffer."
  (let ((buffer-strs
	 (with-current-buffer buf (split-string (buffer-string) "\n" t))))
    (bencher-message (format "BUFFER STR: %s" buffer-strs))
    (bencher-read-tag-values buffer-strs tags () )))

(defun bencher-read-tag-values (data tags csv-accum)
  "read tag-value pairs from a list of strings."
  (if data
      (let* ((key-val (bencher-parse-key-val (car data)))
	     (key (car key-val))       
	     (val (cdr key-val))
	     (key-tag-assoc (assoc key tags)))
	(if (member key tags)
	    (progn
	      (bencher-message (format "starting a tag parse %s" key-val))
	      (bencher-read-tag-values (cdr data) tags  (cons (cons key val) csv-accum)))
	  (bencher-read-tag-values (cdr data) tags csv-accum)))
    csv-accum))


(defun bencher-format-csv-string (csv-format csv-data)
  "using the format list order the contents of csv-data into a string"
  (if csv-format ;; still elements to process  
      (let ((hd (car csv-format))
  	    (tl (cdr csv-format)))
  	(let ((str (bencher-format-csv-string tl csv-data))
  	      (val (assoc hd csv-data)))
	  (if val
	      (if (eq str "")
		  (cdr val) 
		(concat (cdr val) (concat ", " str)))
  	    (concat " ," str))))
    ""))


(defun bencher-output-csv-data (bench)
  "Write csv-data to a buffer named after the currently processed benchmark"
  (let* ((buf-name (bencher-run-unit-csv bench))
	 (buf (get-buffer buf-name))
	 (csv-data (append (bencher-run-unit-csv-data bench)
			   (bencher-run-unit-csv-data-tags bench)))
	 (csv-format (bencher-run-unit-csv-header bench)))
		  
      (bencher-message "Outputing csv")
      (if (= (buffer-size buf) 0) ;; csv buffer is empty
	     (let ((csv-header (mapconcat 'identity csv-format ", "  )))
	       (with-current-buffer buf
		 (insert (concat csv-header "\n"))))
	 
	 
	())
      
      ;; Now contains the header
      (let ((csv-line (bencher-format-csv-string csv-format csv-data)))
	(with-current-buffer buf
	  (goto-char (point-max))	
	  (insert (concat csv-line "\n"))
	  (save-buffer)))))


;; ------------------------------------------------------------
;; Handling of plugins for data processing 
(defun bencher-run-data-processing-plugins (bench plugin-list)
  "Send accumulated data to all functions in the plugin-list"
  (when plugin-list
      (let* ((all-collected-data (append
				  (bencher-run-unit-csv-data bench)
				  (bencher-run-unit-csv-data-tags bench))))
	(dolist (plugin plugin-list ())
	  (message "Hello")
	  (funcall plugin all-collected-data)))))
      
(defun bencher-add-data-processing-plugin (plugin)
  "Add a data processing plugin to the list of plugins"
  (setq bencher-data-processing-plugin-list
       (cons plugin bencher-data-processing-plugin-list)))


;; ------------------------------------------------------------
;; Do the running of benchmarks 
(defun bencher-do-run-benchmarks ()
  "Process enqueued benchmarks. This function is run on a timer"
  (bencher-message (format "There are %s benchmarks to process"
			   (length bencher-scheduled-benchmarks-list)))
  
  (if (not bencher-scheduled-benchmarks-list)
      (cancel-timer bencher-benchmark-run-timer) ; Turn off recurring timer
    (if bencher-running-benchmark
	() ; Benchmark already in progress just return
      (bencher-start-next-benchmark))))

(defun bencher-start-next-benchmark ()
  "Starts the next benchmark in the queue."

  (setq bencher-running-benchmark t)  ; A new bench run is starting
	
  (let* ((bench (car bencher-scheduled-benchmarks-list))
	 (prev-buf (current-buffer))
	 ;; Set up a fresh buffer for each run
	 (buf (generate-new-buffer (bencher-run-unit-name bench)))
	 (curr-date (format-time-string "%Y-%m-%d"))
	 (curr-time (format-time-string "%H-%M-%S")))
    (setq bencher-scheduled-benchmarks-list
	  (cdr bencher-scheduled-benchmarks-list))

    ;; Set up the csv header format 
    (setf (bencher-run-unit-csv-header bench)
	  (append (list "Name")
		  (bencher-run-unit-csv-header bench) ;; Already contains the "ARGS". Maybe change this.
		  (bencher-run-unit-tags bench)
		  bencher-time-tags
		  (list "Date" "Time")))
		  
    ;; Add information to accumulated CSV data
    ;; TODO: Turn this into a function
    (setf (bencher-run-unit-csv-data bench)
	  (cons (cons "Date" curr-date)
		(bencher-run-unit-csv-data bench)))
    (setf (bencher-run-unit-csv-data bench)
	  (cons (cons "Time" curr-time)
		(bencher-run-unit-csv-data bench)))		
	  
    (setf (bencher-run-unit-csv-data bench)
	  (cons (cons "Name"  (bencher-run-unit-name bench)) ;; dotted pair
		(bencher-run-unit-csv-data bench)))
	  
    (set-buffer buf)
    (bencher-message (format "Running benchmark: %s" (bencher-run-unit-name bench)))
    (let ((proc (make-process :name (bencher-run-unit-name bench)
			      :command (append bencher-time-cmd (bencher-run-unit-exec-cmd bench))
			      :buffer buf)))
      (set-process-sentinel
       proc
       (lexical-let ((buf buf)
		     (bench bench)) ;
	 (lambda (process signal)
	   (cond
	    ((equal signal "finished\n")
	       (let ((collected-csv-data (bencher-parse-buffer-for-tags
					  buf
					  (append (bencher-run-unit-tags bench)
						  bencher-time-tags))))
		 (bencher-message (format "Benchmark finished! %s" buf))
		 (bencher-message (format "collected data: %s" collected-csv-data))
		 ;; Add collected csv data to the run-unit (dont know why yet...) 
		 (setf (bencher-run-unit-csv-data-tags bench) collected-csv-data)

		 ;; output both "generated" CSV and collected CSV to buffer
		 (bencher-output-csv-data bench)

		 ;; Let all the data processing plugins handle the data as well.
		 ;; TODO: The CSV output above should be an example of this
		 ;;       kind of plugin
		 (bencher-run-data-processing-plugins
		  bench
		  bencher-data-processing-plugin-list)
		 
		 
		 
		 (setq bencher-running-benchmark nil)
		 ;; Destroy benchmark run output buffer.
		 ;; Todo: Maybe add storing of the buffer to a log file
		 (kill-buffer buf)))
	    ;; Capture all failure cases.
	    ;; TODO: discriminate between them. 
	    (t (bencher-message "Benchmark failed!")
	       (setq bencher-running-benchmark nil)
	       (kill-buffer buf)))))))))
  
;; TODO: Change this into some kind of incremental processing
;;       of the buffer containing the .bench file.
;;       process only a single %% - %% enclosed benchmark at a time
;;       while maintaining knowledge of how far the buffer has been "parsed". 
(defun bencher-do-benchmarks (benches)
  "Enqueue all benchmarks and start the benchmark processing timer func"
  (if bencher-scheduled-benchmarks-list
      (message "Error: There are already scheduled benchmarks") ; An alternative is to just schedule more! 
    (if benches
	(progn
	  (bencher-enqueue-all-benches benches)
	  (if bencher-benchmark-run-timer
	      (cancel-timer bencher-benchmark-run-timer)
	    ())	
	  (setq bencher-benchmark-run-timer
		(run-at-time t 1 #'bencher-do-run-benchmarks))) 
      (message "Error: No benchmarks to run"))))

(defun bencher-enqueue-all-benches (benches)
  "Enqueue all benchmarks, add to scheduled benchmark list"
  (if benches
	(let ((curr-date-time-string (format-time-string "%Y-%m-%d_%H-%M-%S")))
	  (bencher-enqueue-benches (car benches) curr-date-time-string)
	  (bencher-enqueue-all-benches (cdr benches)))
    ()))

(defun bencher-enqueue-benches (bench curr-date-time-string)
  "Expand the varying space of the benchmark and enqueue each instance"
  
  (let* ((varying-strs
	
	  (mapcar (lambda (x) (mapcar 'number-to-string x))
		  (mapcar 'eval (mapcar 'car (mapcar 'read-from-string
			      (mapcar 'cdr (bencher-benchmark-varying bench)))))))
	 (varying-vars
	  (mapcar 'car (bencher-benchmark-varying bench)))
	 (varying-selections
	  (bencher-all-selections varying-strs))
	 (csv-file-name (if (bencher-benchmark-csv bench)
			    (bencher-benchmark-csv bench)
			  (concat (bencher-benchmark-name bench) ".csv")))
	 (csv-output-dir
	  (expand-file-name
	   (concat ".//emacs_bencher//"
		   curr-date-time-string)))
	 
	 (csv-output-file (concat csv-output-dir "//" csv-file-name))
	 (runs-count (if (bencher-benchmark-runs bench)
			 (bencher-benchmark-runs bench)
		       1)))
    
    (if (not (file-directory-p csv-output-dir))
	(make-directory csv-output-dir t)
      ())
    (dolist (elt varying-selections ())
      (let* ((exec-cmd-orig (bencher-benchmark-executable bench))
	     (exec-cmd-str (car (bencher-do-substitutions (list exec-cmd-orig) (mapcar* #'cons varying-vars elt))))
	     (exe-args-exprs (bencher-read-expressions-from-string exec-cmd-str)) 
	     (exe-args-evaled (mapcar 'eval (cdr exe-args-exprs)))
	     (exec-args (mapcar 'number-to-string exe-args-evaled))
	     (arg-bindings (mapcar* #'cons varying-vars exec-args))	     
	     (exec-sym (symbol-name (car exe-args-exprs)))
	     (exec-full (if (string-prefix-p "./" exec-sym) ;; Expand to full filename (full path) 
			    (expand-file-name exec-sym)     ;; if a file in pwd is specified in the .bench file. 
			  exec-sym))                      ;; TODO: Alternatively figure out how to make make-process find executables in pwd. 
	     (exec-cmd (cons exec-full exec-args)))
	(dolist (run-id (number-sequence 0 (- runs-count 1)) t)
	  (let ((run-unit (make-bencher-run-unit))
		(args-csv (bencher-generate-exec-args-csv exec-args))
		(run-id-csv (cons (list "Run-id") (list (cons "Run-id" (number-to-string run-id))))))
	    (setf (bencher-run-unit-run-id run-unit) run-id)
	    (setf (bencher-run-unit-name run-unit) (bencher-benchmark-name bench))
	    (setf (bencher-run-unit-csv run-unit) (find-file-noselect csv-output-file))
	    (setf (bencher-run-unit-exec-args run-unit) arg-bindings)
	    
	    (bencher-append-csv-info run-unit args-csv)
	    (bencher-append-csv-info run-unit run-id-csv)
	    
	    (setf (bencher-run-unit-exec-cmd run-unit) exec-cmd)
	    (setf (bencher-run-unit-tags run-unit) (bencher-benchmark-tags bench))
	    (setq bencher-scheduled-benchmarks-list
		  (cons run-unit bencher-scheduled-benchmarks-list))))))))

;; ------------------------------------------------------------
(defun bencher-generate-exec-args-csv (exec-args)
  "list of arguments to list of pairs (argN . argVal) and the header information"
  (let* ((n (length exec-args))
	 (header (mapcar (lambda (x) (concat "Arg" (number-to-string x)))
			 (number-sequence 0 (- n 1))))
	 (csv-data (mapcar* #'cons 
			    header
			    exec-args)))
    (cons header csv-data)))

(defmacro bencher-append-csv-info (run-unit csv-info)
  "Add information to csv header and accumulated csv data"
  `(progn (bencher-append-csv-header ,run-unit (car ,csv-info))
	  (bencher-append-csv-data   ,run-unit (cdr ,csv-info))))

(defmacro bencher-append-csv-header (run-unit csv-header)
  "Append a list of keys to the csv header line associated with this benchmark"
  `(setf (bencher-run-unit-csv-header ,run-unit)
	 (append ,csv-header (bencher-run-unit-csv-header ,run-unit))))

(defmacro bencher-append-csv-data (run-unit csv-data)
  "Append a list of key-value pairs to the csv data associated with this benchmark"
  `(setf (bencher-run-unit-csv-data ,run-unit)
	 (append ,csv-data (bencher-run-unit-csv-data ,run-unit))))


;; ------------------------------------------------------------
;; User interface
(defun bencher-run-benchmarks-buffer ( &optional buffer)
  "Run benchmarks in buffer, no argument means current-buffer"
  (interactive "bSpecify benchmarks buffer:")
  (cond ((bufferp buffer)
	 (with-current-buffer buffer
	   (let ((str (buffer-string)))
	     (bencher-do-benchmarks
	      (bencher-parse-benchmarks
	       (split-string str "\n"))))))
	(t
	 (let ((str (buffer-string)))
	   (bencher-do-benchmarks
	    (bencher-parse-benchmarks
	     (split-string str "\n")))))))
	
(defun bencher-run-benchmarks-file ( &optional filename)
  "Run benchmarks from file" 
  (interactive "FBenchmarks file:")
  (let ((buf (find-file filename)))
    (bencher-run-benchmarks-buffer buf)
    (kill-buffer buf)))

;; ------------------------------------------------------------
(provide 'bencher)
