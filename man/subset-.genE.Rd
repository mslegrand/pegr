\name{[<-.genE}
\alias{[<-.genE}
\title{Resets the rule values}
\usage{
\method{[}{genE}(parser, rule.id) <- value
}
\value{
peg parser
}
\description{
Resets the rule values
}
\examples{
peg<-new.parser()
peg + "A<-'a'"
peg
peg["A"]<-"A<-'ab'"
peg
peg["A"]<-c("A<-'xx'", des="replace xx by a", act="list('a')")
peg
}

