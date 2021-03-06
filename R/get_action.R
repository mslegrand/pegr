
#' Retrieve an action to the rule specified by rule.id
#' 
#' @param parser, a peg parser produced by  new.parser
#' @param rule.id, a character string naming the rule
#' @return action attached to the specified rule. The action may be
#' may take two forms:
#' \enumerate{
#'  \item the name of a function
#'  \item  a string of text interpreted as a function body with an input parameter consisting of a
#'  list named v, and return value which is also a list.
#' }
#' @examples
#' # Delete all vowels
#' peg<-new.parser()
#' peg<-add_rule(peg, "V<-'a' / 'e' / 'i' / 'o' / 'u' ")
#' peg<-add_rule(peg, "R<-(V / .)+")
#' peg<-set_action(peg, "V", "list()" )
#' g<-function(v){ list(paste(v,collapse='')) }
#' peg<-set_action(peg, "R", g )
#' #see the result
#' value(apply_rule(peg, "R", "cat in the hat", exe=T))
#' # inspect the action for rule "V"
#' get_action(peg, "V")
#' # inspect the action for rule "R"
#' get_action(peg, "R")
#' @export
get_action<-function(pegR, rule.id){
  if(!("pegR" %in% class(pegR))){ stop("first argument not a peg parser", call. = FALSE)}  
  if( rule.id %in% rule_ids(pegR)){
    #actionTxt<-pegR$pegE$.ACTION_NAMES[[rule.id]]  
    actionTxt<-pexGetActionInfo(pegR, rule.id)
  } else {
    stop("cannot get action: invalid rule identifier", call. = FALSE)
  }
  actionTxt
}
