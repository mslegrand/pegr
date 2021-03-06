<!--
%\VignetteEngine{knitr::knitr}
%\VignetteIndexEntry{An Introduction to the pegr package}
-->

An Introduction to the **Pegr** Package
=======================================






Introduction
------------


The  **pegr** package is a package containing a parsing expressing grammer generator, 
together with some tools for managing and debuging sets of rules. Pegr is a recursive decent parser, 
using memoizaton to improve efficiency. The Pegr grammar is the creation of ***Brian Ford***. 
This document is divided up into two major sections, the first just describes the grammar, the second gives a walkthrough on usage within **pegr** 

### The Grammar
The grammar consists of a set of rules ( a rule is also called a definition)
* **Rules** A rule consists of a ruleid, an arrow, and a parsing expresssion.  The ruleid is also called a rule name or a non-termial.
A rule a has the form
$$ RuleId \leftarrow ParsingExpression$$
* **Parsing Expressions**  are built from atoms (aka. literals or terminals) rules, and operators. In pegr, each parsing expression returns a result consisting of three items:
1. A *status* (TRUE for a match, i.e. success, FALSE for no match, i.e. failure )
2. A *position* recording what of the original text was consumed.
3. A *value*, actually a list of one one more values associated with that expression.

#### The following describes the components of a parsing expression:

* **Atoms** are terminal expressions. Atoms consist of literals, ranges, or the dot wild card. Atom may be a single letter 'a' , a word "dog"" or a ranges such as [a-z]. When an atom is matches, the return status is TRUE and a singleton list of value of the match is returned
* **Sequences** A collections of expressions seperated by a blank spaces is called a *sequence*. A sequence is of the form:
   $$e_1 \ e_2\  e_3 \ ... \ e_n$$ where each $e_n$ is an expression. 
For a sequence to match, each component must match successively. For example $$.\ 'a'$$ recognizes any symbol followed by the lower case a. A sequence may be thought of as an and operator. The value returned by sequence is a list consisting of all the values of the individuals components which built up the sequence. 
* **Ordered choice operators**  *Ordered choices operators* are  of the form 
   $$e_1 / e_2 / ... / e_n$$
   Ordered choice returns the value of the first match (if any). If there is no match, the status becomes false, and the expression is rejected. For example, 
   $$'a' \ \ 'b' \ / \ 'c'$$
will match "ab" and "ac".
* **Zero or More operator** The zero or more operator will match if zero or more of occurances of the preceding expression are present. The zero or more operator representation is of the form 
  $$e\*$$
  For example $$'a'\*$$ will match "aaa" and "".
* **One or More operator** The *one or more operator* will match if zero or more of occurances of the preceding expression are present. The *one or more operator* representation is of the form 
  $$e+$$
  For example $$'a'+$$ will match "aaa" but not "".
* **Optional operator** The *optional operator*, or at *most one operator* will match if zero or one of occurances of the preceding expression are present. The optional operator representation is of the form 
  $$e?$$
  For example $$'a'?$$ will match "a" and "", but not "aaa".

* **And Predicate** The *And Predicate* is a look-ahead operator which will match if the next element is a match. It is look ahead in the sense that it does not consume the next expression, it only rejects the current expression if the next expression does not follow. The *And Predicate* is of the form 
  $$ \text{e \&} $$
  For example $$\text{ 'a' \&  'b'} $$ will match "ab" but will return only list(atom="a")

* **Not predicate** The *Not Predicate* is also a look-ahead operator but unlike the *And Predicate*, the *Not Predicate* will fail to match if the next element is a match.  The *Not Predicate* is of the form 
  $$!e$$
  For example $$'a'\ ! \ 'b'$$ will match "ax" but will return only list(atom="a")


Usage Specifics
----------------------------
### Parser
A pegr parser is created by a call to new.parser()

```r
library(pegr)
parser <- new.parser()
```


### Rules
* Rules may be added to a pegr parser by a call to **add_rule(parser, rule)**

```r
parser <- add_rule(parser, "Any<-.")
parser <- add_rule(parser, "A<-'a'")
parser <- add_rule(parser, "B<-'b'")
parser <- add_rule(parser, "C<- A (B / C) ")
parser <- add_rule(parser, "D<- D")  #bad rule: will produce infinite recursion   
```


* The ids or names of all rules in a pegr parser may be retrieved by **rule_ids**

```r
rule_ids(parser)
```

```
## [1] "A"   "Any" "B"   "C"   "D"
```


* Rules contained in a pegr parser may be annotated (commented) by a call to **set_description("ruleid", annotation)**

```r
parser <- set_description(parser, "Any", "Accepts any character")
parser <- set_description(parser, "A", "Accepts a")
parser <- set_description(parser, "B", "Accepts b")
parser <- set_description(parser, "C", "Accepts string of a's terminated by a b")
parser <- set_description(parser, "D", "A very bad rule")
```


* Rules may be removed from a pegr parser by a call to **delete_rule(parse, ruleid)**

```r
parser <- delete_rule(parser, "D")
rule_ids(parser)
```

```
## [1] "A"   "Any" "B"   "C"
```


### Actions
Actions may be attached to an existing rule contained pegr parser by a call to **add_action(parse, action)**
An action may be one of two forms:
* **An r function**:

```r
rule_ids(parser)
```

```
## [1] "A"   "Any" "B"   "C"
```

* **A character string**. This character string should contain code to be executed as the body of a function, where the
input is a list v, and the return value is also a list.

```r
parser <- set_action(parser, "A", "list('A')")  #turn a lower case a to an upper case A
parser <- set_action(parser, "C", "list(paste(v,collapse=''))")  #paste all the characters together
```


### Parsing
Parsing is the act of applying a rule to a string to be parsed and returning a result. 
The rule is specified its ruleid, a character string giving the rules name. That rule becomes the
root of the ensuing parse.
* Parsing is accomplished by
** apply_rule(parser, ruleid, exe=FALSE,, record=FALSE)**. 

```r
res.A <- apply_rule(parser, "A", "a")
res.AAB <- apply_rule(parser, "C", "aab")
res.BAA <- apply_rule(parser, "C", "baab")
```

* Status of a parsing attempt is either **TRUE** for success or **FALSE** for failure. 
The status may be obtained by **status(res)**

```r
c(status(res.A), status(res.AAB), status(res.BAA))
```

```
## [1]  TRUE  TRUE FALSE
```

* The characters consumed by parsing attempt can be gotton from **consumed(res)**

```r
c(consumed(res.A), consumed(res.AAB), consumed(res.BAA))
```

```
## [1] "a"   "aab" ""
```


* Return values of a parsing are lists, which can be gotton from **value(res)**
By default, actions defined by **set_action** are not executed.

```r
# here exe is false, so no action is taken
res.AAB <- apply_rule(parser, "C", "aab")
# so the return is a list of the returns of the component atoms
value(res.AAB)
```

```
## $atom
## [1] "a"
## 
## $atom
## [1] "a"
## 
## $atom
## [1] "b"
```

To execute the actions, supply **exe=TRUE** as a parameter:
* Return values of a parsing are lists, which can be gotton from **value(res)**

```r
# here exe is true, so action is taken
res.AAB <- apply_rule(parser, "C", "aab", exe = TRUE)
# action A capitalizes, action C pastes together
value(res.AAB)
```

```
## $C
## [1] "AAb"
```





### Debugging
Debugging the logic of a grammar can be accomplished testing the component rules starting with the leaves and building up  Also, it's probably better, to start with actions disabled (exe=FALSE) to see first if the general flow is correct. To aid in the analysis of a given parse there are two visualizations tools:
* **tree**: 
* **plot**:
Thesse  visualizaton tools are discussed in the next section

In addition to the visualization tools, we have:
* **a rule stack**, activated by setting the depth via **set_rule_stack_limit** and inspected using **get_rule_stack**
* **a rule debugger**, activated by **debug.pegR**, which allows one to step through and inspect the rules as they are encountered during parsing. Additionally, one may set break points at entry/exit of rules, and skip those rules with no breakpoints set. The ***rule debugger***  debugs  ***RULE LOGIC*** minimal involvement of the low level programming language. 

### Visualization
Visualization consists of showing the nodes visited (and their values) during a parse of an input. This requires that the
**record** flag be set to **TRUE** in the invocation of **apply_rule**. 

```r
# Here record is set to True
peg <- new.parser()
peg <- add_rule(peg, "A<-'a'")
peg <- add_rule(peg, "B<-'b' A")
peg <- add_rule(peg, "C<- A B")
peg <- set_action(peg, "A", "list('X')")
res.ABA <- apply_rule(peg, "C", "aba", exe = TRUE, record = TRUE)
# also, since exe isTrue, so action is taken action A capitalizes, action C
# pastes together
value(res.ABA)
```

```
## $A
## [1] "X"
## 
## $atom
## [1] "b"
## 
## $A
## [1] "X"
```


#### Ways to Visualize
There are two methods of visualization:
* **Tree**: Prints the tree to console

```r
tree(res.ABA)
```

```
## ____C(aba) = list(X, b, X )
##     |____A(a) = list(X )
##     |____B(ba) = list(b, X )
##          |____A(a) = list(X )
```


* **Plot**: Plots the tree to the graphics device

```r
# plot only the ruleids (names)
plot(res.ABA)
```

<img src="figure/debugRulePlot1.png" title="plot of chunk debugRulePlot" alt="plot of chunk debugRulePlot" style="display:block; margin: auto" style="display: block; margin: auto;" />

```r
# plot only the ruleids (names and inputs)
plot(res.ABA, show = c("names", "args"))
```

<img src="figure/debugRulePlot2.png" title="plot of chunk debugRulePlot" alt="plot of chunk debugRulePlot" style="display:block; margin: auto" style="display: block; margin: auto;" />

```r
# plot only the values
plot(res.ABA, show = "vals")
```

<img src="figure/debugRulePlot3.png" title="plot of chunk debugRulePlot" alt="plot of chunk debugRulePlot" style="display:block; margin: auto" style="display: block; margin: auto;" />

```r
# plot all
plot(res.ABA, show = "all", cex = 0.8)
```

<img src="figure/debugRulePlot4.png" title="plot of chunk debugRulePlot" alt="plot of chunk debugRulePlot" style="display:block; margin: auto" style="display: block; margin: auto;" />




