
# This example to extracts all strings enclosed by quotes 
# from the input text and returns those strings as a list
peg<-new.parser()
add_rule(peg, "sq<- \"'\"" )
add_rule(peg, "dq<- '\"'" )
add_rule(peg, "nqp<-  (!(sq / dq) .)+ ")                              
add_rule(peg, "sqp<-sq (!sq .)* sq") #"sqp<-sq (!sq (\\ sq / .)*) sq" 
add_rule(peg, "dqp<-dq (!dq .)* dq")                                  
add_rule(peg, "extract<- ( nqp /  sqp / dqp )+ ")
set_action(peg, "nqp", "list()")
set_action(peg, "dqp", "list(paste(v[2:(length(v)-1)], collapse=''))")
set_action(peg, "sqp", "list(paste(v[2:(length(v)-1)], collapse=''))")

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
