
#first we create the parser
peg<-new.parser()
add_rule(peg, "NUM<-('-')? [0-9]+" )
add_rule(peg, "ATOM <- NUM | ( '(' & SUM & ')' ) ")
add_rule(peg, "DIV <- ATOM '/' PROD" )
add_rule(peg, "MULT <- ATOM '*' PROD" )
add_rule(peg, "PROD <- MULT / DIV / ATOM")
add_rule(peg, "SUB <- PROD '-' SUM")
add_rule(peg, "ADD <- PROD '+' SUM")
add_rule(peg, "SUM <- ADD / SUB /PROD")

#next we set actions to the nodes
set_action(peg, "NUM", "list(as.numeric(paste(v,collapse='')))")
set_action(peg, "ADD", "list( v[[1]]+v[[3]] )" )
set_action(peg, "SUB", "list( v[[1]] - v[[3]] )")
set_action(peg, "MULT","list( v[[1]] * v[[3]] )")
set_action(peg, "DIV","list( v[[1]] / v[[3]] )")

apply_rule(peg, "SUM", "3*4+6/2", debugTree=T)->res
res
tree(res)
plot(res,cex=.6)
