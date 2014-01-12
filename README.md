pegr package
====

This a ***Development Version*** of an R package for generating  a parsing expression grammars  

Installation
-----

From Git using R CMD build
```
install.packages("devtools")
library(devtools)
install_github("mslegrand/pegr")

```
To delete
```
remove.packages("pegr")
```

Motiation
------

The biggest stumbling block to using pegs is writng the rules:
* Making rules with valid PEG syntax
* Making rules that don't hang on input
* Making rules that recognize the input correctly
* Making rules that respond to the input correctly

The prime motivation for this tool is to help overcome these stumbling blocs, by providing an easy way
to enter and evaluate rules. Moreover, we should be able to easliy evaluate the leaf independently,
and then combinations of those leaves, and combination of those, until the final grammar is built. Additionally,
tools for visualizing the computations involved in the application of each rule in the grammar should be easy and 
informative.

Usage
_____

Usage is described in the vignette.   In addition to the examples contained in help,
there are several demos that may provide additional insight.

What's Needed
______

Some obvious  inprovements to consider:
*  More  and more examples
*  A nice user guide (in addition to the vignette)
*  Cleaner code with a cleaner interface
*  Reading and writing peg rules to file. 
*  Detection of rules producing infinite recursion
* Profiling and replacing R functions with C code




