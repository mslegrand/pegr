# This example all identifiers from a rule definition
peg<-new.parser()

peg +
  c("Q1<- \"'\"", "#literal single quote") +
  c("Q2<- '\"'" , "#literal double quote") +
  c("ID<-( [a-z]/[A-Z] )  ( [a-z]/[A-Z]/[0-9] )* ", "{-}", "#Identifier") +
  c("OTH<- !ID !Q1 !Q2 .", "{}", "#Any thing else") +
  c("Q1p<- Q1 (!Q1 (Q1/ .))* Q1 ",  "{}", "#eat single quoted") +
  c("Q2p<- Q2 (!Q2 (Q2/ .))* Q2 ",  "{}", "#eat double quoted") +
  c("EXTRACT<- ( ID / OTH /  Q1p / Q2p )+ ", "#collect all identifers as list")

s<-"A<- A / (B 'c' !ab) / \"'\" / B "

# We apply the rule  extract with the exe option set to be TRUE 
# (exe=FALSE does not execute the actions)
apply_rule(peg, "EXTRACT", s, exe=T)->res

# Value will extract the resulting list of identifiers (one entry per occurance) 
# To easily our resulting strings, we apply cat to each
invisible(sapply(value(res), function(x)cat(x,"\n")))
  
