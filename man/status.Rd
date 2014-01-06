\name{status}
\alias{status}
\title{Returns the Status of a parsing  result}
\usage{
status(res)
}
\arguments{
  \item{A}{result from parsing}
}
\value{
TRUE if successful, FALSE otherwise
}
\description{
Returns the Status of a parsing result
}
\examples{
parser<-new.parser()
#rule to test for string of a's followed by an equal number of b's
add_rule(parser, "S<-'a' S 'b'")
res<-apply_rule(parser, 'S', 'aaaabbbb')
status(res)
res<-apply_rule(parser, 'S', 'aaabbbb')
status(res)
}

