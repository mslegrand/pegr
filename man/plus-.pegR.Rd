\name{+.pegR}
\alias{+.pegR}
\title{Alternative to add_rule.}
\usage{
\method{+}{pegR}(parser, arg)
}
\arguments{
  \item{parser,}{a peg parser produced by new.parser}

  \item{arg,}{a list or vector specififyin a rule:}
}
\description{
Alternative to add_rule.
}
\details{
arg is a list or vector having 1-4 named components:
\itemize{ \item{ rule }{ (Mandatory) A string containing
the peg rule definition For example:
\code{c(Rule="COLD<-'brrr'")}. The 'Rule' label is
optional, but having a rule is mandatory. } \item{ des}{
(optional) A a textual string describing the rule.  For
example: \code{c(Rule="COLD<-'brrr'", des="Polar" )} A
comment must be named.  } \item{ act}{ (optional) an action
specification. For example \code{c(Rule="COLD<-'brrr'",
des="Polar", act=function(v){print("brr"); v} )}, An action
must be named } }
}
\examples{
peg<- new.parser()
peg + "A<-'a'" + "B<-'b'" + "C<-'c'"
#to suppress the output use invisible"
invisible(peg + "A<-'a'" + "B<-'b'" + "C<-'c'")
#now add rule D with action and comment using a named character vector
peg + c("D<-'d'", des="capitalize D", act="list(atom='D')")

#now add rule E with action and comment a unnamed character
peg + c( "E<-'e'", "#double E", "{list('EE')}" )
}

