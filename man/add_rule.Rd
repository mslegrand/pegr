\name{add_rule}
\alias{add_rule}
\title{Adds a rule to the parser}
\usage{
add_rule(parser, rule, des = NULL, act = NULL)
}
\arguments{
  \item{parser,}{a peg parser produced by new.parser}

  \item{rule,}{the rule description, a quoted string that
  defines a rule according to the PEG Grammer}

  \item{des,}{(optional: NULL by default) sets a rule
  description for this rule}

  \item{act,}{(optional: NULL by default) sets an action,
  to be executed by this rule. This NULL by default.}
}
\value{
Status and the rule processed
}
\description{
Adds a rule to the parser, if the rule exists, it is
overwritten, getting a new definition, a new (possibly
empty) description, and a new (possibly empty) action.
}
\examples{
peg<-new.parser()
peg<-add_rule(peg, "X<-x", des="A bad rule (I forgot quotes)")

# Next we inspect the rule
inspect_rule(peg, "X")
# Inspect rule returns the following
# Rule: X
# Def: X<-x
# Com: A bad rule (needs quotes)
# Act:

# Now we replace the rule by overwriting
peg<-add_rule(peg, "X<-'x'", act="list('X')")
# When again inspect, we see
# the definition was fixed (x now is quoted), the description was removed, an action was added.
inspect_rule(peg,"X")
}
\seealso{
For a description of actions see \code{\link{set_action}}
and \code{\link{appy_rule}} )
}

