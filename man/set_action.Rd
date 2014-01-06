\name{set_action}
\alias{set_action}
\title{Attach an action to the rule specified by rule.id}
\usage{
set_action(genE, rule.id, action)
}
\arguments{
  \item{parser,}{a peg parser produced by new.parser}

  \item{rule.id,}{a character string naming the rule}

  \item{action}{to be attached to the specified rule. The
  action may be either a function acceptiong a list as
  input and a list as output or a string of text which may
  be intrepted as a function body that returns a list}
}
\description{
Attach an action to the rule specified by rule.id
}

