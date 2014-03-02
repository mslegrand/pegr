\name{consumed}
\alias{consumed}
\title{Returns The portion ot the input text consumed during a parsing}
\usage{
consumed(res)
}
\arguments{
  \item{res,}{a result obtained from parsing}
}
\value{
The portion ot the input text consumed during the parse

#'
}
\description{
Returns The portion ot the input text consumed during a
parsing
}
\details{
Applying a rule to an input string will consume only the
portion for which there is a match. If no match, is
consumed is "", then the status is FALSE, but even if there
is a partial match, the status will be TRUE. In debugging
it may be useful to see how far the parser succeeded. If a
complete is required then end the rule with \bold{!.},
meaning no more charactes.
}
\examples{
peg<-new.parser()
peg<-add_rule(pe)

#A returns TRUEfor "aab" but consumes on the first 2 charactes
peg<-new.parser()
peg<-add_rule(peg, "A <- ('a' A) / 'a' ")
apply_rule(peg, "A", "aab")->res
status(res
consumed(res)

#All returns FALSE on "aab"
peg<-add_rule(peg, "All <- A !. ")
apply_rule(peg, "All", "aab")->res
status(res)
consumed(res)
#All return TRUE on "aaa"
apply_rule(peg, "All", "aaa")->res
status(res)
consumed(res)

#Another approach: change A to
#This now returns FALSE on "aab
peg<-add_rule(peg, "A<- ('a' A ) / ('a' !.)")
apply_rule(peg, "A", "aab")->res
status(res)
consumed(res)
#But returns true on: "aaa"
apply_rule(peg, "A", "aaa")->res
status(res)
consumed(res)
}

