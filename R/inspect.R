
#' Inspects a given rule contained within a parser
#' 
#' This is used to see what a comprises a given rule. 
#' @param parser, a peg parser
#' @param rule_id, a rule idenitifier, a.k.a. rule name
#' @return ruleStruct, a container which when printed will produce a
#' a summary of that rule
#' 
#' @examples
#' peg<-new.parser()
#' peg<-add_rule(peg, "DOG<-'fido' / 'spot' / 'rover'/ 'buddy'")
#' peg<-set_action(peg, "DOG", "list('bark')")
#' peg<-set_description(peg, "DOG", "sound of dog")
#' inspect_rule(peg, "DOG")
#' @export
inspect_rule<-function(peg, rule.id){
  if( !( "pegR" %in% class(peg) ) ){ stop("first argument not a parser", call. = FALSE) }   
  rs<-pexGetRuleStructure(peg, rule.id)
  rs
}
