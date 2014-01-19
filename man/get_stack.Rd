\name{get_stack}
\alias{get_stack}
\title{Returns the call stack when the stop level was exceeded}
\usage{
get_stack(pegR)
}
\arguments{
  \item{parser,}{a peg parser produced by new.parser}
}
\value{
call stack, a data frame show the sequence of calls
encounterd prior to the exceeding the stop level
}
\description{
Returns the call stack when the stop level was exceeded
}
\examples{
peg<-new.parser()
set_stop_level(peg, 20)
add_rule(peg, "A<-B" )
add_rule(peg, "B<-C")
add_rule(peg, "C<-A") # A vicious circle of calls resulting in infinite recursion
apply_rule(peg, "A", "x") # will stop at 20
get_stack(peg)
}

