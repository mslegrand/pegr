\name{inspect_rule}
\alias{inspect_rule}
\title{Inspects a given rule contained within a parser}
\usage{
inspect_rule(peg, rule.id)
}
\arguments{
  \item{parser,}{a peg parser}

  \item{rule_id,}{a rule idenitifier, a.k.a. rule name}
}
\value{
ruleStruct, a container which when printed will produce a a
summary of that rule
}
\description{
This is used to see what a comprises a given rule.
}
\examples{
peg<-new.parser()
peg<-add_rule(peg, "DOG<-'fido' / 'spot' / 'rover'/ 'buddy'")
peg<-set_action(peg, "DOG", "list('bark')")
peg<-set_description(peg, "DOG", "sound of dog")
inspect_rule(peg, "DOG")
}

