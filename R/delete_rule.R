#' Deletes the given rule form the parser.
#' 
#' @param parser, a peg parser produced by  new.parser
#' @param rule.id, a character string naming the rule
#' 
#' #' # Deleteing a rule
#' @examples
#' peg<-new.parser()
#' add_rule(peg,  "A<-'a'")
#' add_rule(peg,  "B<-'b'")
#' peg
#' delete_rule(peg, "A")
#' peg
#' @export
delete_rule<-function(pegR, rule.id){
  #delete rule 
  if(!("pegR" %in% class(pegR))){ stop("first argument not a peg parser", call. = FALSE)}  
  pexDeleteRule(pegR, rule.id)
}
