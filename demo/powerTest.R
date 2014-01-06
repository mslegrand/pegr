
parser<-new.parser()
add_rule(parser, "S <- &(A 'c') 'a'+ B !('a'/'b'/'c')")
add_rule(parser, "A <- 'a' A? 'b'")
add_rule(parser, "B <- 'b' B? 'c'")
apply_rule(parser, "S", "aaabbbccc")->res
status(res)
