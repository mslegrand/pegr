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
  add_rule(peg, "A<-'a'")
  add_rule(peg, "B<-'b'")
  add_rule(peg, "D<-'d'")
  add_rule(peg, "C<-'c'")
  add_rule(peg,"ROOT<-A B C D")
  apply_rule(peg,"ROOT","abcd")->res
  res #invokes print.PEGResult
}

