\name{+.genE}
\alias{+.genE}
\title{Alternative to add_rule.}
\usage{
\method{+}{genE}(parser, arg)
}
\description{
Alternative to add_rule.
}
\examples{
peg<- new.parser()
peg + "A<-'a'" + "B<-'b'" + "C<-'c'"
#to suppress the output use invisible"
invisible(peg + "A<-'a'" + "B<-'b'" + "C<-'c'")
}

