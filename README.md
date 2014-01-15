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
-------

Usage is described in the vignette.   In addition to the examples contained in help,
there are several demos that may provide additional insight.

We currently supply 2 approaches to usage:
* traditional form: commands of the form **add_rule(peg, rule, ...)**, **apply_rule(peg, rule.id, input.txt, ...)**
* operator form: commands of the form **peg + rule** , **peg\[rule](input.txt)**

Design Considerations (Open for Your Input/Discussion)
---------
There are at least 3 design considerations which are still open, and which I would love feedback on:
* **peg + rule + rule** for adding rules may not be the best choice, currently considering using **peg <= rule <= rule** as a replacement
* **peg\[rule.id](input.txt)** violates the usual usage of \[] in R. ([] returns an attachedRule object, not a Peg, and this [] does not vectors of rule.ids greater than length 1) A more proper version would be to ues **peg\[\[rule.id]](text.input)**. But this is messier and perhaps less intuitive. Also matrices violate the return type rule, so maybe this isn't so bad.
* pegs are really environments with wrappers, so the usual copy doesnot work. I will probably change this.

What's Needed
-------

Some obvious  inprovements to consider:
*  More  and more examples
*  A better user guide (in addition to the vignette)
*  Cleaner code with a cleaner interface
*  Reading and writing peg rules to file. 
*  Detection of rules producing infinite recursion
*  Profiling and replacing R functions with C code




