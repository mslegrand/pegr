\name{set_action}
\alias{set_action}
\title{Attach an (optional) action to a specified rule}
\usage{
set_action(pegR, rule.id, action)
}
\arguments{
  \item{parser,}{a peg parser produced by new.parser}

  \item{rule.id,}{a character string naming the rule}

  \item{action}{to be attached to the specified rule. The
  action may be may take three forms: \enumerate{ \item a
  function accepting a list as input and a list as output
  \item a string of text to be interpreted as a function
  body with an input parameter consisting of a list named
  v, and return value which is also a list. If the list
  which is the return value has NULL for names, (i.e. no
  names have been assigned to the list members) then the
  members of the list are assigned the names given by the
  rule id)

  \item NULL, in which case the action associated with the
  given rule is removed. }}
}
\description{
Attach an (optional) action to a specified rule
}
\examples{
#Capitalize all occurances of 'a' using inline actions
peg<-new.parser()
add_rule(peg, "A<-'a'")
add_rule(peg, "R<-(A / .)+")
set_action(peg, "A", "list('A')")
set_action(peg, "R", "list(paste(v, collapse=''))" )
value(apply_rule(peg, "R", "cat in the hat", exe=T))
}

