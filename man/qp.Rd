\name{qp}
\alias{qp}
\title{A simple tool to examine a parsing expression in isolation}
\usage{
qp(expression, arg, debugTree = TRUE)
}
\arguments{
  \item{arg,}{text string to apply expression to.}

  \item{debugTree=TRUE,}{By default TRUE, so one can
  immediately graph (plot) or print (tree)}
}
\value{
PEGResult, an object giving the result
}
\description{
This can be quite useful in getting to know the PEG syntax
}
\examples{
# A simple choice operator
qp("'a' / 'b'", "ab")
# A simple sequence operator
qp("'a' 'b'", "ab")
# A lookahead not operator
qp("'a' !'b'", "ab" )
qp("'a' !'b'", "ac" )
# An lookahead and operator
qp("'a' & 'b'", "ab")
qp("'a' & 'b'", "ac")
# An optional operator
qp("'a' 'b'?", "ab")
qp("'a' 'b'?", "ac")
qp("'a'? 'b'", "ab")
qp("'a'? 'b'", "ba")
}

