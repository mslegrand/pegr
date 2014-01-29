#' Gets a description of a given rule
#' 
#' Descriptions are like comments for rules, it is a good practice to comment 
#' your rules after you add them. See \code{\link{set_description}}
#' 
#' @param rule.id, a character string naming the rule
#' @param parser, a peg parser produced by  new.parser
#' @return description, a character string describing the parser
#' @examples
#' pegR<-new.parser()
#' add_rule(pegR, "A<-'a'")
#' set_description(pegR, "A", "recognizes 'a'")
#' get_description(pegR, "A")
#' @export
get_description<-function(pegR, rule.id){
  if(!("pegR" %in% class(pegR))){ stop("first argument not a peg parser", call. = FALSE)}  
  if( rule.id %in% rule_ids(pegR)){
    #description<-pegR$GET_DESCRIPTION( rule.id)
    description<-pexGetDescription(pegR, rule.id)
    return(description)
  } else {
    stop("cannot get description: invalid rule identifier", call. = FALSE)
  }  
}
