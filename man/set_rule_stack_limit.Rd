\name{set_rule_stack_limit}
\alias{set_rule_stack_limit}
\title{Sets  restriction on the maximum call depth of rules}
\usage{
set_rule_stack_limit(pegR, stop.level.limit)
}
\arguments{
  \item{parser,}{a peg parser produced by new.parser}

  \item{stop.level.limit,}{a restriction on the number of
  calls (levels) before stopping a parse}
}
\description{
Sets restriction on the maximum call depth of rules
}
\details{
Since a rule can call other rules or even itself, the depth
of a calling sequence can grow and potentially be
infinited. By setting the stop level one is restricting
that depth, and hence can detect possible infinite
recursive calls. To inspect the calling sequence of those
rules when the stop level is exceed, use
\code{\link{get_rule_stack}}
}
\examples{
peg<-new.parser()
set_rule_stack_limit(peg, 10)
add_rule(peg, "A<-A" ) #an infinite recursive call
apply_rule(peg, "A", "x") #the input is irrevelant, throws error
}
\seealso{
\code{\link{get_rule_stack}},
\code{\link{unset_rule_stack_limit}}
}

