\name{value}
\alias{value}
\title{Returns the value of a parsing  result}
\usage{
value(res)
}
\value{
A list containing the values computed
}
\description{
Returns the value of a parsing result
}
\examples{
# Select the matching pair of from the alternatives
peg<-new.parser()
peg<-add_rule(peg, "R<-('a'/'c') ('b' / 'd')")
apply_rule(peg, "R", 'ad')->res
value(res)

# double all occurances of all vowels
peg<-new.parser()
peg<-add_rule(peg, "V<-'a' / 'e' / 'i' / 'o' / 'u' ")
peg<-add_rule(peg, "C<-(!V .)")
peg<-add_rule(peg, "A<- (V / C)+" )
peg<-set_action(peg, "V", "c(v[1],v[1])" )
peg<-set_action(peg, "A", "list(paste0(v,collapse=''))" )
apply_rule(peg, "A", "the big bad wolf", exe=TRUE)->res
value(res)[1]
}

