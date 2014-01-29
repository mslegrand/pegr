\name{set_description}
\alias{set_description}
\title{Attach an (optional) description to a specified rule}
\usage{
set_description(pegR, rule.id, description)
}
\arguments{
  \item{parser,}{a peg parser produced by new.parser}

  \item{rule.id,}{a character string naming the rule}

  \item{description,}{a text string describing the rule}
}
\description{
A description should be used to comment a given rule
}
\examples{
pegR<-new.parser()
add_rule(pegR, "A<-'a'")
set_description(pegR, "A", "recognizes 'a'")
}

