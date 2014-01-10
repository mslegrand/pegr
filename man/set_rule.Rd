\name{set_rule}
\alias{set_rule}
\title{Sets a rule in the parser}
\usage{
set_rule(parser, rule)
}
\arguments{
  \item{parser,}{a peg parser produced by new.parser}

  \item{rule,}{a quoted string that defines a rule
  according to the PEG Grammer}
}
\value{
Status and the rule processed
}
\description{
This only sets the rule and definition. Thus for existing
rules, this overwrites the rule definition, but leaves rule
actions and descriptions unaffected. For non-existing rules
this is equivalent to add_rule(parser, rule, descrip=NULL,
action=NULL) See \code{\link{add_rule}}
}
\examples{
peg<-new.parser()
# Add rule A to recognize 'a' and return list('A') as it's value
add_rule(peg, "A<-'a'", act="list('A')")
value(apply_rule(peg, 'A', 'a', exe=TRUE))
set_rule(peg, "A<-'b'")
# Now A will only recognize 'b', so it will now fail on input 'a'
status(apply_rule(peg, 'A', 'a', exe=TRUE))
# However, A not recognizes b, but still returs list('A') as it's value
value(apply_rule(peg, 'A', 'b', exe=TRUE))
}

