pegr package
====

This a ***Development Version*** of an R package for generating a peg parser. Peg is short for
**parsing expression grammer**, whose specification can be found in \link{http://bford.info/pub/lang/peg}. 
This software generates parsers based on that specification. By stay as close as possible to the 
original specification, we hope to provide an implementation agnostic solution to parsing. 

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

One of the biggest issues in using a peg generator to produce a parser is getting the rules "right"."
In particular:
* Ensuring the rules have a valid PEG syntax
* Ensuring the rules don't hang on a given input.
* Ensuring the rules accept the correct input
* Ensuring the rules perform the right actions.

The prime motivation for this tool is concentrate on the rules themselves and getting them "right" with as
few of distractions as possible. This means that when debugging, it is the rules and not R code which should be of 
concern. For this reason, it was decided rules should be textual, and not R code.  
Thus the parser is built from rules entered as text.


Features of this Tool
-------
*  an easy way to enter and evaluate rules. 
*  an easy way to break down and test component rules
*  the ability to inspect how the final evalution was obtained.
*  the abiltiy to plot the parsing tree of an evaluation.
*  a way to limit the depth of rule calls, and to inspect that stack when exceeded (to help detect potential hangs)
*  a debugger specific to debugging the rules.  It allows to set breakpoints step through the execution of the rules while inspect returned by the excution of each rule. This debugger debugs the rules, not R code!

Usage
-------

Usage is described in the vignette.   In addition to the examples contained in help,
there are several demos that may provide additional insight.

We currently supply 2 approaches to usage:
* traditional form: commands of the form **add_rule(peg, rule, ...)**, **apply_rule(peg, rule.id, input.txt, ...)**
* operator form: commands of the form **peg + rule** , **peg\[rule](input.txt)**

Design Considerations (Open for Your Input/Discussion)
---------
There are a couple of design considerations which are still open, and which I would love feedback on:
* **peg + rule + rule** for adding rules may not be the best choice of operator
* pegs are really environments with wrappers, so the usual copy doesnot work. I will probably change this.

What's Needed
-------

Some obvious  improvements to consider:
*  More and more examples
*  Enhancing the tutorial
*  Reading and writing peg rules to file. 
*  Profiling and replacing R functions with C code




