\name{new.parser}
\alias{new.parser}
\title{Creates an instance of a new PEG parser.}
\usage{
new.parser(peg.data.frame = NULL, record.mode = FALSE, action.exe = FALSE)
}
\arguments{
  \item{peg.data.frame,}{a data frame with rules to
  populate the parser. (default is NULL) A peg.data.frame
  consists of the following fields: \itemize{
  \item{rule.id} \emph{(mandatory)}, {The column containing
  the rule's id. \emph{(NA values not allowed)}}
  \item{rule.source} \emph{(mandatory)}, {The column
  containing the rule's definition \emph{(NA values not
  allowed)}} \item{rule.description}, \emph{(optional)}
  {The column containing any rules description (may have NA
  values) } \item{action.specification}, \emph{(optional)}
  {The column containing the rule specifiation. May be
  either a valid inline action or the name of an existing
  rule (Set to NA if e no action is associated with this
  rule.)} }}

  \item{record.mode,}{when set to TRUE, will keep a record
  to display with \code{\link{tree}} or \code{\link{plot}}.
  (default is FALSE)}

  \item{action.exe,}{when set to TRUE, all actions will be
  executed unless overridden by command line. (default is
  FALSE)}
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
peg<-add_rule(parser, "Any<-.")
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
\seealso{
\code{\link{as.data.frame}}
}
\keyword{PEG}
\keyword{grammer}
\keyword{parser}

