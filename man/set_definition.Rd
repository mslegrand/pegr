\name{set_definition}
\alias{set_definition}
\title{Reset rule.definition in the parser}
\usage{
set_definition(parser, rule.id, rule.definition)
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
This only sets the rule definition (source). This
overwrites the old rule definition,leaving rule actions and
descriptions unaffected. To add rules see
\code{\link{add_rule}}
}
\examples{
peg<-new.parser()
# Add rule A to recognize 'a' and return list('A') as it's value
add_rule(peg, "A<-'a'", act="list('A')")
value(apply_rule(peg, 'A', 'a', exe=TRUE))
set_definition(peg, "A<-'b'")
# Now A will only recognize 'b', so it will now fail on input 'a'
status(apply_rule(peg, 'A', 'a', exe=TRUE))
# However, even though  Arecognizes b, but the return value is still  list('A').
value(apply_rule(peg, 'A', 'b', exe=TRUE))
}

