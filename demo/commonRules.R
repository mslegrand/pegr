peg<-new.parser()
peg + 
  c("EOF<- !.", "#End of Input" ,"{}") +
  c("EOL<- '\n' / EOF", "#End of Line","{}") +
  c("WS<- ' '/'\t'/EOL", "#White space", "{}") +
  c("ALPHA<- [a-z]/[A-Z]", "#Any letter") +
  c("WORD<-ALPHA+ WS", "#Any word", "{-}") +
  c("DIGIT<-[0-9]", "#Digit" ) +
  c("NUM<-DIGIT+ ('.' DIGIT+)? WS", "#Number", "{v<-paste1(v); list(as.numeric(v[[1]]))}")

df<-as.data.frame(peg)


