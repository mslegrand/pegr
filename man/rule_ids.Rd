\name{rule_ids}
\alias{rule_ids}
\title{Lists all Rules contained in the parser}
\usage{
rule_ids(pegr)
}
\arguments{
  \item{parser,}{a peg parser produced by new.parser}
}
\description{
Returns a charactor vector giving all the defined rules
contained in the parser.
}
\examples{
peg<-new.parser()
peg<-add_rule(peg, "I<-'Ice'")
peg<-add_rule(peg, "A<-'Age'")
peg<-add_rule(peg, "S<-'Scrat'")
rule_ids(peg) #c("I", "A", "S")
}

