\name{unset_stop_level}
\alias{unset_stop_level}
\title{Unsets a restriction the maximum levels (call depth) of a parse}
\usage{
unset_stop_level(pegR)
}
\arguments{
  \item{parser,}{a peg parser produced by new.parser}
}
\description{
Unsets the retriction that was set by
\code{\link{set_stop_level}}
}
\examples{
peg<-new.parser()
set_stop_level(peg, 10)
add_rule(peg, "A<-'a' A / ''" ) #consumes all a's at the beginning of a string
apply_rule(peg, "A", "aaaaaaaaaaaax") #there are 12 a's but will stop at 10
#error, stop level exceeded
unset_stop_level(peg)
apply_rule(peg, "A", "aaaaaaaaaaaax") #now succeeds
}
\seealso{
\code{\link{get_stack}}, \code{\link{set_stop_level}}
}

