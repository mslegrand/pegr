pegr package
====

This a ***Development Version*** of an R package for generating  a parsing expression grammars  

Installation
-----

From Git using R CMD build
```
install.packages("devtools")
library(devtools)
install_github("pegr")

```
To delete
```
remove.packages("pegr")
```

Motiation
------

The biggest stumbling block in using pegs is debugging.  And the most
likely source of an error comes from a grammar (i.e. set of rules) that fail
to do what was intended on some particular text input. 
Thus the prime motivation for this tool is to provide an easy way
to analyse where the grammar breaks down for given input.

Usage
_____

Usage is described in the vignette.   In addition to the examples contained in help,
there are several demos that may provide additional insight.

What's Needed
______

Some obvious  items:
[ ]  More  and better examples
[ ]  Reading and writing peg rules to file
[ ]  Dectection of rules producing infinite recursion
[ ]  Profiling and replacing R functions with C
[ ]  A nice user guide (in addition to the vignette
[ ]  Cleaner code with a cleaner interface



