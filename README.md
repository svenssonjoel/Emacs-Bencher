# Emacs-Bencher (Work in progress)
Use Emacs to run benchmarks and gather statistics into a csv buffer/file. 

# In progress or semi-working
  * Parse output buffer for tag-value pairs. 
  * CSV generation. Works, but needs to be extended with parsing built in tags and additional info.
  
# TODO
  * Learn to code LISP.
  * Add error checking to many places. 
  * Break up large functions into small.
  * Log output buffers.
  * Makefile projects and compile-time argument support.
  * Editing mode for .bench files (with run-benchmark-closeset-to-cursor function and run-all-benchmarks function).
  * Think of a .bench file format that makes more sense.
  * Add functionality to repeat a benchmark N times (give the user data from each run in the CSV).
  * Potentially the .bench file can specify a fold operation over the N repeats of each benchmark run. 
  * Add success/failure code when "shelling" out to run benchmarked executable. Maybe add a retry counter and a max-retries number.
  * The benchmark output should go to a temp buffer (one per "run") for tag parsing. Rather than using process-filters. 
  * Add functionality for running the benchmarks on a remote machine via emacs deamon.


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
