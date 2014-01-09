\name{add_rule}
\alias{add_rule}
\title{add_rule
Adds a rule to the parser}
\usage{
add_rule(parser, rule, des = NULL, act = NULL)
}
\arguments{
  \item{parser,}{a peg parser produced by new.parser}

  \item{rule,}{a quoted string that defines a rule
  according to the PEG Grammer}

  \item{des,}{a rule description}

  \item{act,}{an action, to be executed by this rule (see
  \code{\link{set_action}} and \code{\link{appy_rule}} )}
}
\value{
Status and the rule processed
}
\description{
add_rule Adds a rule to the parser
}

