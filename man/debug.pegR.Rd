\name{debug.pegR}
\alias{debug.pegR}
\title{Turn on the pegR rule debugger}
\usage{
debug.pegR(peg)
}
\description{
The pegR rule debugger allows one to step through as the
rules as they are invoked.
}
\details{
When applying a given rule to to a text input string, that
rule may call other rules which in turn may call other
rules, ad nausem. The peg rule debugger allows one to step
through that calling sequence, examining the state of the
parsing both upon entering a rule and and exiting a rule.
}
\seealso{
\code{\link{undebug.pegR}}
}

