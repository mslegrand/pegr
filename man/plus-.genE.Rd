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
#now add rule D with action and comment
peg + c("D<-'d'", des="capitalize D", act="list(atom='D')")
}

