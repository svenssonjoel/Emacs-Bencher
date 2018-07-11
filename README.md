# Emacs-Bencher (Work in progress)
Use Emacs to run benchmarks and collect data

# TODO
  * Learn to code LISP.
  * Parse output buffer for tag-value pairs.
  * Add error checking to many places. 
  * Break up large functions into small.
  * CSV generation.
  * Log output buffers.
  * Makefile projects and compile-time argument support.
  * Editing mode for .bench files (with run-benchmark-closeset-to-cursor function and run-all-benchmarks function).
  * Learn if it is possible to run stuff on a remote Emacs (There is some emacs-deamon). 


# Goals
Be able to run sets of performance (or other) tests on executables (to begin with) from inside emacs.



# .bench file syntax example

```
%%
name: test2
varying: a '(1 2 3 4 5 6)
varying: b '(1 2)
executable: ./bench1 (* a 5) (+ b 10)
tags: '("TAG0" "TAG1") 
%%
```
* name: specifies a name for this set of benchmark runs.
* varying: specifies a variable and a space (an elisp expression evaluating to a list of values) for it to range over.
* executable: specifies that an executable called "bench1" should be executed with two arguments
that range over the lists specified as varying. Each combination of one value from the a list and one from the b constitutes a run. Lisp expressions in this string are evaluated and the result of evaluation passed to the executable.
* tags: specifies a list of names of tags. If the program under benchmarking outputs a string 'TAG: value', value will be parsed and stored in output csv. The separating colon in the benchmark output tag line is required. 
