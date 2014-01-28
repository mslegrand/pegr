\name{new.parser}
\alias{new.parser}
\title{Creates an instance of a new PEG parser.}
\usage{
new.parser(df = NULL, record.mode = FALSE)
}
\value{
Returns a new instance of a PEG parser
}
\description{
Creates an instance of a new PEG parser.
}
\examples{
parser<-new.parser()
add_rule(parser, "Any<-.")
rule_ids(parser)  # returns "Any"
}
\keyword{PEG}
\keyword{grammer}
\keyword{parser}

