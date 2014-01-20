\name{set_record_model}
\alias{set_record_model}
\title{Sets the peg to recording mode}
\usage{
set_record_model(peg, on = TRUE)
}
\arguments{
  \item{peg}{parser produced by new.parser()}

  \item{on,}{when set to TRUE, turns on the recording
  status.}
}
\description{
When recording is on (TRUE), applying a peg rule to input a
record of the tree of nodes (and their values) is saved for
use by \code{\link{tree}} or \code{\link{plot}}. This value
is a default value and can be override by setting the
record option during the rule application
}
\seealso{
\code{\link{tree}} \code{\link{plot}}
\code{\link{apply_rule}}
}

