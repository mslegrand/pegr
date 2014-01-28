pegr package
====

This an R package for generating a peg parser. Peg is short for
**parsing expression grammer**. The specification for the Peg grammar can be found at http://bford.info/pub/lang/peg. 
This software generates parsers based on that specification. By staying as close as possible to the 
original specification, we hope to provide a parsing solution which is language agnostic. That is, our hope is 
for a tool allowing one to concentrate on rule building, and not the nuances of a particular programming language.

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

Motivation
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
*  Easy **Interactive**  makes easy 
    *  adding rules to the peg
    *  applying a rule to text input
    *  check  and test component rules
*  Easy inspection of the evaluation tree with the **tree** command. Provides a textual representation of how the final evalution was obtained.
*  Graphing the evalutation tree with the **plot** command. 
*  A way to limit the depth of calls to rules by using the **set_rule_stack_limit** command, and then by using **get_rule_stack**, the stack may be inspected whenever the limit is exceeded (This is to help detect the source of  hangning due to infinite recursion)
*  **A  specialized rule debugger** to ease the effort of debugging rule logic.  The debugger allows one to inpsect all rules in the order in which they applied to the input: both when the rule is entered and exited. This allows one to inspect return status (whether or not the rule accepts the input), the text consumed, and any values produced by the rule.  ***The rule debugger is for debugging the RULE SET LOGIC, not R code!***
* A tutorial included to help get started.
Usage
-------

Usage is described in the vignette.   In addition to the examples contained in help,
there are several demos that may provide additional insight.

We currently supply 2 approaches to usage:

1. Traditional form: commands of the form 
   * **add_rule(peg, rule, ...)** : *adds (attaches) a rule to the peg*
   * **apply_rule(peg, rule.id, input.txt, ...)**  : *applys an attached rule to the input text*
2. Operator form: commands of the form 
   * **peg + rule1 + rule 2 + ...** : *adds (attaches) rules to the peg*
   * **peg\[\[rule,id]](input.txt)** : *applys an attached rule to the input text*

Design Considerations (Open for Your Input/Discussion)
---------
There are a couple of design considerations which are still open, and which I would love feedback on:
* **peg + rule1 + rule2** for adding rules may not be the best choice of operator
* pegs are really environments with wrappers, so the usual copy does not work.

What's Needed
-------

Some obvious  improvements to consider:
*  More and more examples
*  Enhancing the tutorial
*  Profiling and replacing R functions with C code
*  Replacing environments with lists to make make copy work?




