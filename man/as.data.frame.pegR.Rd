\name{as.data.frame.pegR}
\alias{as.data.frame.pegR}
\title{produces data.frame consisting of peg rules}
\usage{
\method{as.data.frame}{pegR}(peg, ...)
}
\arguments{
  \item{parser,}{a peg parser produced by new.parser}
}
\value{
data.frame of rules
}
\description{
produces data.frame consisting of peg rules
}
\examples{
peg<-new.parser()
peg +
 c("A<-'a'", des="a 2 A", action="'A'") +
 c("B<-'b'", des="b does nothtng")
as.data.frame(peg)
}

