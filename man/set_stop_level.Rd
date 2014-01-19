\name{set_stop_level}
\alias{set_stop_level}
\title{Sets  restriction on the maximum levels (call depth) of a parse}
\usage{
set_stop_level(pegR, stop.level.limit)
}
\arguments{
  \item{parser,}{a peg parser produced by new.parser}

  \item{stop.level.limit,}{a restriction on the number of
  calls (levels) before stopping a parse}
}
\description{
Sets restriction on the maximum levels (call depth) of a
parse
}
\details{
Since a rule can call other rules or even itself, the depth
of a calling sequence can grow and potentially be
infinited. By setting the stop level one is restricting
that depth, and hence can detect possible infinite
recursive calls. To inspect the calling sequence of those
rules when the stop level is exceed, use
\code{\link{get_stack}}
}
\examples{
peg<-new.parser()
set_stop_level(peg, 10)
add_rule(peg, "A<-A" ) #an infinite recursive call
apply_rule(peg, "A", "x") #the input is irrevelant, throws error
}
\seealso{
\code{\link{get_stack}}, \code{\link{unset_stop_level}}
}

