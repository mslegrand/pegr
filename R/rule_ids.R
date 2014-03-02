#' Lists all Rules contained in the parser
#' 
#' Returns a charactor vector giving all the defined rules contained
#' in the parser.
#' 
#' @examples
#' peg<-new.parser()
#' peg<-add_rule(peg, "I<-'Ice'")
#' peg<-add_rule(peg, "A<-'Age'")
#' peg<-add_rule(peg, "S<-'Scrat'")
#' rule_ids(peg) #c("I", "A", "S")
#' @param parser, a peg parser produced by  new.parser
#' @export
rule_ids<-function(pegr){
  pexGetIDs(pegr)
}
