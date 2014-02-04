peg<-new.parser()
peg + 
  c("EOF<- !.", "#End of Input" ,"{}") +
  c("EOL<- '\n' / EOF", "#End of Line","{}") +
  c("WS<- ' '/'\t'/EOL", "#White space", "{}") +
  c("ALPHA<- [a-z]/[A-Z]") +
  c("WORD<-ALPHA+ WS", "any word", "{-}") +
  c("DIGIT<-[0-9]", "digit" ) +
  c("NUM<-DIGIT+ ('.' DIGIT+)? WS", "v<-paste1(v); list[as.numeric(v[[1]])")

df<-as.data.frame(peg)
