\name{status}
\alias{status}
\title{Returns the Status of a parsing  result}
\usage{
status(res)
}
\arguments{
  \item{A}{result from parsing, i.e. the result form
  applying a rule to a text string input.}
}
\value{
TRUE if successful, FALSE otherwise
}
\description{
Returns the Status of a parsing result
}
\examples{
parser<-new.parser()
#This rule to tests for string of a's followed by an equal number of b's
add_rule(parser, "S<-'a' S 'b'")

#test against 3 a's followed b 3 b's returns TRUE
apply_rule(parser, 'S', 'aaaabbbb')
status(res)

#test again with input of 3 a's followed bu 3 b's,
#this returns False
res<-apply_rule(parser, 'S', 'aaabbbb')
status(res)

See also
}

