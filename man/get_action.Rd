\name{get_action}
\alias{get_action}
\title{Retrieve an action to the rule specified by rule.id}
\usage{
get_action(pegR, rule.id)
}
\arguments{
  \item{parser,}{a peg parser produced by new.parser}

  \item{rule.id,}{a character string naming the rule}
}
\value{
action attached to the specified rule. The action may be
may take two forms: \enumerate{ \item the name of a
function \item a string of text interpreted as a function
body with an input parameter consisting of a list named v,
and return value which is also a list. }
}
\description{
Retrieve an action to the rule specified by rule.id
}
\examples{
# Delete all vowels
peg<-new.parser()
peg<-add_rule(peg, "V<-'a' / 'e' / 'i' / 'o' / 'u' ")
peg<-add_rule(peg, "R<-(V / .)+")
peg<-set_action(peg, "V", "list()" )
g<-function(v){ list(paste(v,collapse='')) }
peg<-set_action(peg, "R", g )
#see the result
value(apply_rule(peg, "R", "cat in the hat", exe=T))
# inspect the action for rule "V"
get_action(peg, "V")
# inspect the action for rule "R"
get_action(peg, "R")
}

