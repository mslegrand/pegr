\name{[[.pegR}
\alias{[[.pegR}
\title{An alternative to apply_rule}
\usage{
\method{[[}{pegR}(parser, rule.id)
}
\description{
An alternative to apply_rule
}
\examples{
# The simplest example: a parser that only accepts a single character 'a'
# First create a new pegR
peg<-new.parser()
# Next we add the rule to the peg
peg + "A<-'a'"
# Next apply the rule to the string "a"
peg[['A']]("a")->res
# to see the result  print(res)
res
}

