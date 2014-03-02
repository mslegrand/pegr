
parser<-new.parser()
parser<-add_rule(parser, "S <- &(A 'c') 'a'+ B !('a'/'b'/'c')")
parser<-add_rule(parser, "A <- 'a' A? 'b'")
parser<-add_rule(parser, "B <- 'b' B? 'c'")
apply_rule(parser, "S", "aaabbbccc")->res
status(res)
