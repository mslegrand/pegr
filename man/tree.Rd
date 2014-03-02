\name{tree}
\alias{tree}
\title{Print Tree}
\usage{
tree(res)
}
\arguments{
  \item{res,}{a result obtained from parsing with
  record=TRUE}
}
\description{
Prints a tree representing the result of applying a rule to
a text input, when that result was obtained with the record
parameter set to TRUE
}
\examples{
peg<-new.parser()
  peg<-add_rule(peg, "A<-'a'")
  peg<-add_rule(peg, "B<-'b'")
  peg<-add_rule(peg, "D<-'d'")
  peg<-add_rule(peg, "C<-'c'")
  peg<-add_rule(peg,"ROOT<-A B C D")
  apply_rule(peg,"ROOT","abcd", record=TRUE)->res
  tree(res)
}

