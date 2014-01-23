\name{delete_rule}
\alias{delete_rule}
\title{Deletes the given rule form the parser.}
\usage{
delete_rule(pegR, rule.id)
}
\arguments{
  \item{parser,}{a peg parser produced by new.parser}

  \item{rule.id,}{a character string naming the rule

  #' # Deleteing a rule}
}
\description{
Deletes the given rule form the parser.
}
\examples{
peg<-new.parser()
add_rule(peg,  "A<-'a'")
add_rule(peg,  "B<-'b'")
peg
delete_rule(peg, "A")
peg
}

