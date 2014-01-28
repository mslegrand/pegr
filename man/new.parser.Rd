\name{new.parser}
\alias{new.parser}
\title{Creates an instance of a new PEG parser.}
\usage{
new.parser(data.frame = NULL, record.mode = FALSE)
}
\arguments{
  \item{peg.data.frame,}{a data frame with rules to
  populate the parser. (default is NULL) A peg.data.frame
  consists of the following fields: * *rule.id*, column
  containing the rule's id (mandatory) * *rule.source*, a
  column containing the rule's definition (mandatory) *
  *rule.description* a column to contain a rules
  description (optional, and may contain NA) *
  *action.type* a column indicating the type of action
  ("Inline" or "External") (optional if there are no
  actions, may contain NA) * *action.specification*, a
  column containg either a valid inline action or the name
  of an existing rule (optional if there are no actions)}

  \item{record.mode,}{when set, will keep a record to
  display with tree or plot. (default is FALSE)}
}
\value{
Returns a new instance of a PEG parser
}
\description{
Creates an instance of a new PEG parser.
}
\examples{
#Create an empty parser
parser<-new.parser()
add_rule(parser, "Any<-.")
rule_ids(parser)  # returns "Any"

#Create a parser from a data.frame
fn<-function(x){list()}
df<-data.frame(
rule.id=c('A','B'),
rule.source=c("A<-'a'", "B<-'b'"),
rule.description=c("aaa",NA),
action.type=c("Inline","External"),
action.specification=c("list()", "fn"),
stringsAsFactors=FALSE)
peg<-new.parser(df)
}
\keyword{PEG}
\keyword{grammer}
\keyword{parser}

