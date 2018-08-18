# Emacs-Bencher
Emacs-Bencher automates the running of a program over a space of inputs while
gathering statistics about for example running time into a csv buffer/file. 

Like the Haskell-based [HSBencher](https://hackage.haskell.org/package/hsbencher)
tool, Emacs-Bencher looks for the occurence of user defined "tags" in the
output stream of the running program. Tags are of the form "key: value" and are
parsed by Emacs-Bencher and added to the csv result. 

# Installation example

1. Clone the repository for example in the .emacs.d directory.
```
cd ~/.emacs.d 
git clone https://github.com/svenssonjoel/Emacs-Bencher.git'
```

2. Add the following to your .emacs file.
```
(add-to-list 'load-path "~/.emacs.d/Emacs-Bencher/")
(require 'bencher)
``` 
# .bench file syntax example
In Emacs-Bencher, one or more benchmarks are specified in a file (or Emacs buffer) as a set of colon separated key value pairs.
Benchmarks are separated using %%. For some keys the associated value can be expressed using an embedded elisp expression. 

```
%%
name: test2
csv: test2.csv
runs: 5
varying: a '(1 2 3 4 5 6)
varying: b '(1 2)
executable: ./bench1 (* a 5) (+ b 10)
tags: '("TAG0" "TAG1") 
%%
```
* name: specifies a name for this set of benchmark runs.
* csv: specifies a name for a csv output buffer (and file, todo).
* runs: specifies how many times the benchmark should run at each varying setting. 
* varying: specifies a variable and a space for it to range over (an elisp expression evaluating to a list of values) for it to range over.
* executable: specifies that an executable called "bench1" should be executed with two arguments
that range over the lists specified as varying. Each combination of one value from the a list and one from the b constitutes a run. Lisp expressions in this string are evaluated and the result of evaluation passed to the executable.
* tags: specifies a list of names of tags. If the program under benchmarking outputs a string 'TAG: value', value will be parsed and stored in output csv. The separating colon in the benchmark output tag line is required. 

# Running a set of benchmarks
There are two Emacs-Bencher functions for starting a set of benchmarks.

1. bencher-run-benchmarks-file "\path\to\file.bench"
2. bencher-run-benchmarks-buffer buffer

These can be run using 'M-x' or 'M-:'

# Extensible through data processing "plugins"

Additional methods of processing the benchmark result data can be added to the bencher-data-processing-plugin-list.
A "plugin" is function taking a list of key-value pairs as input. The example below just outputs the results
of each benchmark to the Messages buffer. 

```
;; Example of a data processing plugin
(defun bencher-dummy-echo-to-messages (values)
   "dummy"
   (message (format "DUMMY: %s" values)))

(setq bencher-data-processing-plugin-list
       (cons #'bencher-dummy-echo-to-messages
 	     bencher-data-processing-plugin-list))
```

# Extensible using pre- and post-benchmark "information harvesters"

An information harvester is a function that returns a key-value pair when given
the argument nil. If the argument to the harvester is t it should return its key.
All harvesters are run for each benchmark run. The key-value pairs returned by harvesters
are added to the CSV output (and passed to whatever data processing plugins there are).

pre- and post-harvesters are added to the following lists:
```
bencher-pre-information-harvester-list
bencher-post-information-harvester-list
```

Example harvester that grabs machine name:
```
(defun bencher-uname-n-information-harvester (just-header)
  (let ((key "Node"))
    (if just-header
	key
      (with-temp-buffer
	(call-process "uname" 'nil (current-buffer) 'nil "-n")
	(cons key
	      (bencher-newline-to-space (buffer-string)))))))
``` 


# Example benchmark and csv output

```
%%
name: sleep
runs: 2
varying: a  '(1 2 3 4 5 6)
executable: sleep a
%%
```

```
Name, Run-id, Arg0, TimeReal, TimeUser, TimeSys, TimeNCS, Date, Time
sleep, 1, 6, 6.00, 0.00, 0.00, 0, 2018-08-12, 08-12-12
sleep, 0, 6, 6.00, 0.00, 0.00, 0, 2018-08-12, 08-12-19
sleep, 1, 5, 5.00, 0.00, 0.00, 0, 2018-08-12, 08-12-26
sleep, 0, 5, 5.00, 0.00, 0.00, 0, 2018-08-12, 08-12-32
sleep, 1, 4, 4.00, 0.00, 0.00, 0, 2018-08-12, 08-12-38
sleep, 0, 4, 4.00, 0.00, 0.00, 0, 2018-08-12, 08-12-43
sleep, 1, 3, 3.00, 0.00, 0.00, 0, 2018-08-12, 08-12-48
sleep, 0, 3, 3.00, 0.00, 0.00, 0, 2018-08-12, 08-12-52
sleep, 1, 2, 2.00, 0.00, 0.00, 0, 2018-08-12, 08-12-56
sleep, 0, 2, 2.00, 0.00, 0.00, 0, 2018-08-12, 08-12-59
sleep, 1, 1, 1.00, 0.00, 0.00, 0, 2018-08-12, 08-13-02
sleep, 0, 1, 1.00, 0.00, 0.00, 0, 2018-08-12, 08-13-04
```

# TODO
  * Learn to code LISP.
  * BUG: Reset does not seem to "reset" enough after aborting an ongoing benching session (by reload of file)
  * Add error checking to many places. 
  * Log output buffers.
  * Makefile projects and compile-time argument support.
  * Editing mode for .bench files (with run-benchmark-closest-to-cursor function and run-all-benchmarks function).
