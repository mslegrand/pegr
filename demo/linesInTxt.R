#' Return a list of all lines in a text
#' 
#' @example
peg<-new.parser()
peg + c("EOL<- '\n'" , "{}")
peg + c("EOF<- !.")
peg + c("EOLF<-EOL /EOF")
peg + c("LINE<- (!EOLF .)+ EOLF", "{-}")
peg + c("LINES<- LINE+")
txt<-"hello there
this is my greatest
and funniest example"

peg[["LINES"]](txt, exe=T)->res
value(res)
