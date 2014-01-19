\name{[[<-.pegR}
\alias{[[<-.pegR}
\title{Reset an existing rule}
\usage{
\method{[[}{pegR}(parser, rule.id) <- value
}
\arguments{
  \item{parser,}{the peg parser containing the rule to be
  modified}

  \item{rule.id,}{the identifier of the rule to be
  modified}

  \item{value,}{the modifications to be applied. Can be
  either a vector or a list.}
}
\value{
peg parser
}
\description{
Used to Modify the rule definition and or description or
value of of an exiting rule
}
\examples{
peg<-new.parser()
peg + "A<-'a'"
peg
peg[["A"]]<-"A<-'ab'"
peg
peg[["A"]]<-c("A<-'xx'", des="replace xx by a", act="list('a')")
peg
}
