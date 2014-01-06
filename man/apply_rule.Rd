\name{apply_rule}
\alias{apply_rule}
\title{Invoke the parserto parse using the rule.id as the root.}
\usage{
apply_rule(parser, rule.id, arg, exe = FALSE, debugTree = FALSE)
}
\arguments{
  \item{parser,}{a peg parser produced by new.parser}

  \item{rule.id,}{a character string naming the rule}

  \item{arg,}{a character string to be parsed}

  \item{exe,}{a flag indicate whether actions should be
  performed. when false, no actions will be executed}

  \item{debugTree,}{a flag which when set produces tree of
  snapshots of all nodes visited during the parsing
  process}
}
\description{
Invoke the parserto parse using the rule.id as the root.
}

