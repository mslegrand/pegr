\name{set_action}
\alias{set_action}
\title{Attach an (optional) action to a specified rule}
\usage{
set_action(genE, rule.id, action)
}
\arguments{
  \item{parser,}{a peg parser produced by new.parser}

  \item{rule.id,}{a character string naming the rule}

  \item{action}{to be attached to the specified rule. The
  action may be may take three forms: \enumerate{ \item a
  function accepting a list as input and a list as output
  \item a string of text to be interpreted as a function
  body with an input parameter consisting of a list named
  v, and return value which is also a list.  \item NULL, in
  which case the action associated with the given rule is
  removed. }}
}
\description{
Attach an (optional) action to a specified rule
}
\examples{
#Capitalize all occurances of t using function calls
peg<-new.parser()
add_rule(peg, "T<-'t'")
add_rule(peg, "R<-(T / .)+")
f<-function(v){return(list('T'))}
set_action(peg, "T", f)
g<-function(v){return(list(paste(v,collapse='')))}
set_action(peg, "R", g )
value(apply_rule(peg, "R", "cat in the hat", exe=T))

#Capitalize all occurances of A using inline actions
peg<-new.parser()
add_rule(peg, "A<-'a'")
add_rule(peg, "R<-(A / .)+")
set_action(peg, "A", "list('A')")
set_action(peg, "R", "list(paste(v, collapse=''))" )
value(apply_rule(peg, "R", "cat in the hat", exe=T))
}

