\name{print.PEGResult}
\alias{print.PEGResult}
\title{prints the final result of applying a rule to text input}
\usage{
\method{print}{PEGResult}(res)
}
\description{
prints the final result of applying a rule to text input
}
\note{
for readability, quotes are left out when printing the
list.
}
\examples{
peg<-new.parser()
  peg<-add_rule(peg, "A<-'a'")
  peg<-add_rule(peg, "B<-'b'")
  peg<-add_rule(peg, "D<-'d'")
  peg<-add_rule(peg, "C<-'c'")
  peg<-add_rule(peg,"ROOT<-A B C D")
  apply_rule(peg,"ROOT","abcd")->res
  res #invokes print.PEGResult
}

