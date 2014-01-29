\name{get_description}
\alias{get_description}
\title{Gets a description of a given rule}
\usage{
get_description(pegR, rule.id)
}
\arguments{
  \item{rule.id,}{a character string naming the rule}

  \item{parser,}{a peg parser produced by new.parser}
}
\value{
description, a character string describing the parser
}
\description{
Descriptions are like comments for rules, it is a good
practice to comment your rules after you add them. See
\code{\link{set_description}}
}
\examples{
pegR<-new.parser()
add_rule(pegR, "A<-'a'")
set_description(pegR, "A", "recognizes 'a'")
get_description(pegR, "A")
}

