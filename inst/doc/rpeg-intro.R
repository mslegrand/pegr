
## ----setup, include=FALSE------------------------------------------------
library(knitr)
opts_chunk$set(out.extra='style="display:block; margin: auto"', fig.align="center")


## ----methods-------------------------------------------------------------
library(pegr)
parser <- new.parser()


## ----add_rule------------------------------------------------------------
parser <- add_rule(parser, "Any<-.")
parser <- add_rule(parser, "A<-'a'")
parser <- add_rule(parser, "B<-'b'")
parser <- add_rule(parser, "C<- A (B / C) ")
parser <- add_rule(parser, "D<- D") #bad rule: will produce infinite recursion   


## ----rule_ids------------------------------------------------------------
rule_ids(parser)


## ----set_description-----------------------------------------------------
parser <- set_description(parser, "Any", "Accepts any character")
parser <- set_description(parser, "A", "Accepts a")
parser <- set_description(parser, "B", "Accepts b")
parser <- set_description(parser, "C", "Accepts string of a's terminated by a b")
parser <- set_description(parser, "D", "A very bad rule")


## ----delete_rule---------------------------------------------------------
parser <- delete_rule(parser, "D")
rule_ids(parser)


## ----add_action_function-------------------------------------------------
rule_ids(parser)


## ----add_action_char_string----------------------------------------------
parser<-set_action(parser, "A", "list('A')") #turn a lower case a to an upper case A
parser<-set_action(parser, "C", "list(paste(v,collapse=''))") #paste all the characters together


## ----apply_rule----------------------------------------------------------
apply_rule(parser, "A", "a" )->res.A  
apply_rule(parser, "C", "aab")->res.AAB
apply_rule(parser, "C", "baab")->res.BAA


## ----status.ResA---------------------------------------------------------
c(status(res.A) ,status(res.AAB), status(res.BAA) )


## ----consume-------------------------------------------------------------
c(consumed(res.A),  consumed(res.AAB), consumed(res.BAA))


## ----valueF--------------------------------------------------------------
# here exe is false, so no action is taken
apply_rule(parser, "C", "aab")->res.AAB 
# so the return is a list of the returns of the component atoms
value(res.AAB)


## ----valueT--------------------------------------------------------------
#here exe is true, so action is taken
apply_rule(parser, "C", "aab", exe=TRUE)->res.AAB 
# action A capitalizes, action C pastes together
value(res.AAB) 


## ----debugRuleT----------------------------------------------------------
# Here record is set to True
apply_rule(parser, "C", "aab", exe=TRUE, record=TRUE)->res.AAB 
# also, since exe isTrue, so action is taken
# action A capitalizes, action C pastes together
value(res.AAB) 


## ----debugRuleTree-------------------------------------------------------
tree(res.AAB) 


## ----debugRulePlot-------------------------------------------------------
#plot only the ruleids (names)
plot(res.AAB) 
#plot only the ruleids (names and inputs)
plot(res.AAB, show=c("names", "args")) 
#plot only the values
plot(res.AAB, show="vals")
#plot all
plot(res.AAB, show="all")


