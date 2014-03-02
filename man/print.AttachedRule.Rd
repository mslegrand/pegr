\name{print.AttachedRule}
\alias{print.AttachedRule}
\title{Prints an attached rule}
\usage{
\method{print}{AttachedRule}(attached.rule)
}
\description{
Prints an attached rule
}
\examples{
peg<-new.parser()
peg<-add_rule(peg, "R<-'ab'")
print(peg[["R"]])
}

