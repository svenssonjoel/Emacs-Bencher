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
name: test
varying: a (1 2 3 4 5 6)
varying: b (1 2)
executable: ./bench1 (* %a 5) (+ %b 10)
tag: TAG0 int
tag: TAG1 double
%%
```
* name: specifies a name for this set of benchmark runs.
* varying: specifies a variable and a space for it to range over.
* executable: specifies that an executable called "bench" should be executed with two arguments
that range over the lists specified as varying.
* tag: specifies the name and the type of tagged data in the program-under-benchmarking output stream.

