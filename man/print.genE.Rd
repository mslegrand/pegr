\name{print.genE}
\alias{print.genE}
\title{Prints the rules contained in the PEG}
\usage{
\method{print}{genE}(parser)
}
\arguments{
  \item{peg}{parser produced by new.parser()}
}
\description{
Prints the rules contained in the PEG
}
\examples{
peg<-new.parser()
add_rule(peg, "NUM<-[0-9]+ (. [0-9] )?" )
set_description(peg, "NUM", "A Number")
add_rule(peg, "FT<-NUM ' '* ft")
set_description(peg, "FT"," measurement in feet")
# Now print the print the rules
print(peg)
}

