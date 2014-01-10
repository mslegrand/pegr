\name{tree}
\alias{tree}
\title{Print Tree}
\usage{
tree(res)
}
\arguments{
  \item{res,}{a result obtained from parsing with
  debugTree=TRUE}
}
\description{
Prints a tree representing the result of applying a rule to
a text input, when that result was obtained with the
debugTree parameter set to TRUE
}
\examples{
peg<-new.parser()
  add_rule(peg, "A<-'a'")
  add_rule(peg, "B<-'b'")
  add_rule(peg, "D<-'d'")
  add_rule(peg, "C<-'c'")
  add_rule(peg,"ROOT<-A B C D")
  apply_rule(peg,"ROOT","abcd", debugTree=TRUE)->res
  tree(res)
}

