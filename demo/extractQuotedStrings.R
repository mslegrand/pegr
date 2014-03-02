
# This example to extracts all strings enclosed by quotes 
# from the input text and returns those strings as a list
peg<-new.parser()
peg<-add_rule(peg, "sq<- \"'\"" )
peg<-add_rule(peg, "dq<- '\"'" )
peg<-add_rule(peg, "nqp<-  (!(sq / dq) .)+ ")                              
peg<-add_rule(peg, "sqp<-sq (!sq .)* sq") #"sqp<-sq (!sq (\\ sq / .)*) sq" 
peg<-add_rule(peg, "dqp<-dq (!dq .)* dq")                                  
peg<-add_rule(peg, "extract<- ( nqp /  sqp / dqp )+ ")
peg<-set_action(peg, "nqp", "list()")
peg<-set_action(peg, "dqp", "list(paste(v[2:(length(v)-1)], collapse=''))")
peg<-set_action(peg, "sqp", "list(paste(v[2:(length(v)-1)], collapse=''))")

# Our input text contains a single quoted string (with double quotes inside)
# and a double quoted string (with a single quote inside)
s<-" noquote '\"sin\"gles' \"double's\" "
# We cat instead print for better inspection
cat(s)
# We apply the rule  extract with the exe option set to be TRUE 
# (exe=FALSE does not execute the actions)
apply_rule(peg, "extract", s, exe=T)->res
# Value extract the resulting list of strings 
# To easily our resulting strings, we apply cat to each
invisible(sapply(value(res), function(x)cat(paste(x,"\n"))))
