pegr package
====

This an R package for generating a peg parser. Peg is short for
**parsing expression grammer**. The specification for the Peg grammar can be found at http://bford.info/pub/lang/peg. 
This software generates parsers based on that specification. By staying as close as possible to the 
original specification, we hope to provide a parsing solution which is language agnostic. That is, our hope is 
for a tool allowing one to concentrate on rule building, and not the nuances of any particular programming language.

Motivation
------

One of the biggest issues in using a peg generator to produce a parser is getting the rules "right"."
In particular:
* Ensuring the rules have a valid PEG syntax.
* Ensuring the rules don't hang on a given input. (infinite recursion)
* Ensuring the rules accept the correct input.
* Ensuring the rules perform the right actions.

This tool is built with one principle in mind: Let the user ***concentrate on the getting rules right***   with as
few of distractions as possible. This means we want debug  ***RULES*** , not R code. Rules are entered as text- not code, debugging steps through the rules- not code, breakpoint are for rules- not code, stacks are stacks of rules -not code. To sum it up: ***It's the RULES*** that rule. 


Features of this Tool
-------
*  **Interactivity:**  makes it easy to 
    *  adding rules to the peg
    *  applying a rule to text input
    *  checking  and testing rule components
*  Easy inspection of the evaluation tree with the **tree** command. Provides a textual representation of how the final evalution was obtained.
*  Graphing the evalutation tree with the **plot** command. 
*  A way to limit the depth of calls to rules by using the **set_rule_stack_limit** command, and then by using **get_rule_stack**, the stack may be inspected whenever the limit is exceeded (This is to help detect the source of  hangning due to infinite recursion)
*  **A  specialized rule debugger** to ease the effort of debugging rule logic.  The debugger allows one to inpsect all rules in the order in which they applied to the input: both when the rule is entered and exited. This allows one to inspect return status (whether or not the rule accepts the input), the text consumed, and any values produced by the rule.  ***The rule debugger is for debugging the RULES,  not code!***
* Parser ***summary*** providing
   * A listing of any rules used (by other rules) but not defined in the rule set.
   * A listing of all rules that may either directly or indirectly call them selves (recursive)
   * A listing of all rules that can only play the role of a root (not called by any rule)
   * A listing of all rules that can only play the role of a leaf (does not call any rule)
* A tutorial included to help get started.
* Writing rules to a data.frame. The rules can then be read back in upon initialization. Thus by rbinding data frames rules sets can be saved to file or combined into a single peg. This allows for building base 'libraries' (in the form of data.frames) which can be loaded upon startup and then extended for customized solutions.

Usage
-------

Usage is described in the **vignette** and in the tutorial under **inst/documents/tutorial.pdf**.   Various examples contained in help the online help (rd docs). For additional insight, there is a growing number of demos located inside the **demos** directory.

The steps for usage are:

_**Step 1:**_ **Construct** a new peg parser: 
```
peg<-new.parser()
```
Note: An data.frame argument may be supplied, which will load a set of rules stored as a data.frame

_**Step 2:**_ **Add** rules to the parser (The "{...}" are optional actions.)
```
peg + c("A<- 'a' .", "{-}") + c("X<-.", "{}") + "R<- A / X"
```

_**Step 3:**_ **Apply** a rule to an input string
```
example: peg[['R']]('abacda')
```
We currently supply 2 approaches to usage:

1. Traditional form: commands of the form 
   * **add_rule(peg, rule, ...)** : *adds (attaches) a single rule to the peg*
   * **apply_rule(peg, rule.id, input.txt, ...)**  : *applys an attached rule to the input text*
2. Operator form: commands of the form (also shown above)
   * **peg + rule1 + rule 2 + ...** : *adds (attaches) one or more rules to the peg*
   * **peg\[\[rule,id]](input.txt)** : *applys an attached rule to the input text*


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



