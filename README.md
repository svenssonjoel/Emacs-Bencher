# Emacs-Bencher
Use Emacs to run benchmarks and gather statistics into a csv buffer/file. 

# Inspiration
The Haskell-based [HSBencher](https://hackage.haskell.org/package/hsbencher) tool.

# Installation example
Clone the repository for example in the .emacs.d directory
```
cd ~/.emacs.d 
git clone https://github.com/svenssonjoel/Emacs-Bencher.git'
```
Then add the following to your .emacs file.
```
(add-to-list 'load-path "~/.emacs.d/Emacs-Bencher/")
(require 'bencher)
``` 

# TODO
  * Learn to code LISP.
  * Add error checking to many places. 
  * Log output buffers.
  * Makefile projects and compile-time argument support.
  * Editing mode for .bench files (with run-benchmark-closeset-to-cursor function and run-all-benchmarks function).
  * Think of a .bench file format that makes more sense.
  * Add functionality to repeat a benchmark N times (give the user data from each run in the CSV).
  * Potentially the .bench file can specify a fold operation over the N repeats of each benchmark run. 
  * Add checking of success/failure code when "shelling" out to run benchmarked executable. Maybe add a retry counter and a max-retries number.
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


# Example benchmark and csv output

```
%%
name: sleep
varying: a  '(1 2 3 4 5 6)
executable: sleep a
%%
```

```
Name, TimeReal, TimeUser, TimeSys, TimeNCS
sleep,  6.00,  0.00,  0.00,  0
sleep,  5.00,  0.00,  0.00,  0
sleep,  4.00,  0.00,  0.00,  0
sleep,  3.00,  0.00,  0.00,  0
sleep,  2.00,  0.00,  0.00,  0
sleep,  1.00,  0.00,  0.00,  0
```