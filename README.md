# Emacs-Bencher
Use Emacs to run benchmarks and gather statistics into a csv buffer/file. 

# Inspiration
The Haskell-based [HSBencher](https://hackage.haskell.org/package/hsbencher) tool.

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

# TODO
  * Learn to code LISP.
  * Add error checking to many places. 
  * Log output buffers.
  * Makefile projects and compile-time argument support.
  * Editing mode for .bench files (with run-benchmark-closeset-to-cursor function and run-all-benchmarks function).
  * Add checking of success/failure code when "shelling" out to run benchmarked executable. Maybe add a retry counter and a max-retries number.

# .bench file syntax example

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
Name, Arg0, TimeReal, TimeUser, TimeSys, TimeNCS, Date, Time
sleep, 6, 6.00, 0.00, 0.00, 0, 2018-08-07, 08-28-47
sleep, 5, 5.00, 0.00, 0.00, 0, 2018-08-07, 08-28-54
sleep, 4, 4.00, 0.00, 0.00, 0, 2018-08-07, 08-29-00
sleep, 3, 3.00, 0.00, 0.00, 0, 2018-08-07, 08-29-05
sleep, 2, 2.00, 0.00, 0.00, 0, 2018-08-07, 08-29-09
sleep, 1, 1.00, 0.00, 0.00, 0, 2018-08-07, 08-29-12
```
