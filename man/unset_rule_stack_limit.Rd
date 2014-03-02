\name{unset_rule_stack_limit}
\alias{unset_rule_stack_limit}
\title{Unsets a restriction the maximum lcall depth of rules}
\usage{
unset_rule_stack_limit(pegR)
}
\arguments{
  \item{parser,}{a peg parser produced by new.parser}
}
\description{
Unsets the retriction that was set by
\code{\link{set_rule_stack_limit}}
}
\examples{
\dontrun{
peg<-new.parser()
set_rule_stack_limit(peg, 10)
peg<-add_rule(peg, "A<-'a' A / ''" ) #consumes all a's at the beginning of a string
apply_rule(peg, "A", "aaaaaaaaaaaax") #there are 12 a's but will stop at 10
#error, rule stack depth limit exceeded
unset_rule_stack_limit(peg)
apply_rule(peg, "A", "aaaaaaaaaaaax") #now succeeds
}
}
\seealso{
\code{\link{get_rule_stack}},
\code{\link{set_rule_stack_limit}}
}

