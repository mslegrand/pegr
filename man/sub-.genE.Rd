\name{[.genE}
\alias{[.genE}
\title{An alternative to apply_rule}
\usage{
\method{[}{genE}(parser, rule.id)
}
\description{
examples peg<-new.parser() add_rule(peg, "R<-'ab')
peg["R"]('ab') #equivalent to **apply_rule(peg, "R",
'ab')**
}

