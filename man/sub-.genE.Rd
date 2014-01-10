\name{[.genE}
\alias{[.genE}
\title{An alternative to apply_rule}
\usage{
\method{[}{genE}(parser, rule.id)
}
\description{
An alternative to apply_rule
}
\examples{
peg<-new.parser()
add_rule(peg, "R<-'ab'")
#next apply the rule
peg["R"]('ab')
}

