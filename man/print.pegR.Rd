\name{print.pegR}
\alias{print.pegR}
\title{Prints the rules contained in the PEG}
\usage{
\method{print}{pegR}(parser)
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
peg<-set_description(peg, "NUM", "A Number")
peg<-add_rule(peg, "FT<-NUM ' '* ft")
peg<-set_description(peg, "FT"," measurement in feet")
# Now print the print the rules
print(peg)
}

