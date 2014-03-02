#' Attach an (optional) description to a specified rule
#' 
#' A description
#' should be used to comment a given rule
#' 
#' @param parser, a peg parser produced by  new.parser
#' @param rule.id, a character string naming the rule
#' @param description, a text string describing the rule
#' @examples 
#' pegR<-new.parser()
#' peg<-add_rule(pegR, "A<-'a'")
#' pegR<-set_description(pegR, "A", "recognizes 'a'")
#' @export
set_description<-function(pegR, rule.id, description){
  if(!("pegR" %in% class(pegR))){ stop("first argument not a peg parser", call. = FALSE)}  
  if( rule.id %in% rule_ids(pegR)){
    #pegR$SET_DESCRIPTION(rule.id, description)
    peg<-pexClonePegR(pegR) #to make it look like R
    pexSetDescription(peg, rule.id, description)
    return(peg)
    #invisible(TRUE)
  } else {
    stop("cannot set description: invalid rule identifier", call. = FALSE)
  }
}
