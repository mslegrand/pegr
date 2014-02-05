# This example to extracts all strings NOT enclosed by quotes 
# from the input text and returns those strings as a list
peg<-new.parser()
peg +
  c("Q1<- \"'\"", "#literal single quote") +
  c("Q2<- '\"'" , "#literal double quote") +
  c("NQp<- ( !(Q1 /Q2)  . )+", "{-}", "#collect unquoted") +
  c("Q1p<- Q1 (!Q1 (Q1/ .))* Q1 ",  "{}", "#eat single quoted") +
  c("Q2p<- Q2 (!Q2 (Q2/ .))* Q2 ",  "{}", "#eat double quoted") +
  c("EXTRACT<- ( NQp /  Q1p / Q2p )+ ", "#collect all unquoted pieces as list")

# Our input text contains a single quoted string (with double quotes inside)
# and a double quoted string (with a single quote inside)
s<-" noquote '\"sin\"gles'  noquote2 \"double's\"  nwquote3"

cat(s) # We cat instead print for better viewing


# We apply the rule  extract with the exe option set to be TRUE 
# (exe=FALSE does not execute the actions)
apply_rule(peg, "EXTRACT", s, exe=T)->res

# Value extract the resulting list of strings 
# To easily our resulting strings, we apply cat to each
invisible(sapply(value(res), function(x)cat(x,"\n")))


