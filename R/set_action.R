#' Attach an (optional) action to a specified rule
#' 
#' @param parser, a peg parser produced by  new.parser
#' @param rule.id, a character string naming the rule
#' @param action to be attached to the specified rule. The action may be
#' may take three forms:
#' \enumerate{
#'  \item  a function accepting a list as input and a list as output
#'  \item  a string of text to be interpreted as a function body with an input parameter consisting of a
#'  list named v, and return value which is also a list.
#'  \item  NULL, in which case the action associated with the given rule is removed.
#' }
#' @examples
#' #Capitalize all occurances of t using function calls
#' peg<-new.parser()
#' add_rule(peg, "T<-'t'")
#' add_rule(peg, "R<-(T / .)+")
#' f<-function(v){return(list('T'))}
#' set_action(peg, "T", f)
#' g<-function(v){return(list(paste(v,collapse='')))}
#' set_action(peg, "R", g )
#' value(apply_rule(peg, "R", "cat in the hat", exe=T))
#' 
#' #Capitalize all occurances of A using inline actions
#' peg<-new.parser()
#' add_rule(peg, "A<-'a'")
#' add_rule(peg, "R<-(A / .)+")
#' set_action(peg, "A", "list('A')")
#' set_action(peg, "R", "list(paste(v, collapse=''))" )
#' value(apply_rule(peg, "R", "cat in the hat", exe=T))
#' 
#' @export
set_action<-function(pegR, rule.id, action){
  #TODO:  ( expression?)
  #TODO: refactor using switch?
  if(!("pegR" %in% class(pegR))){ stop("first argument not a peg parser", call. = FALSE)}  
  if( rule.id %in% rule_ids(pegR)){
    if(class(action)=="character"){
      
      actionFn<-paste("function(v){",action,"}")
      #pegR$pegE$.ACTION[[rule.id]]<-eval(parse(text=action))  
      pexSetAction(pegR, rule.id, eval(parse(text=actionFn)))
      #pegR$pegE$.ACTION_NAMES[[rule.id]]<-c("Inline:",action)
      actionInfo<-c("Inline:",action)
      pexSetActionInfo(pegR, rule.id, actionInfo)
    } else if (class(action)=="function"){
      #pegR$pegE$.ACTION[[rule.id]]<-action 
      pexSetAction(pegR, rule.id, action)
      #pegR$pegE$.ACTION_NAMES[[rule.id]]<-c("External Function:", deparse(substitute(action)))
      actionInfo<-c("External Function:", deparse(substitute(action)))
      pexSetActionInfo(pegR, rule.id, actionInfo)
    } else if (is.null(action)){
      #pegR$pegE$.ACTION[[rule.id]]<-NULL
      #pegR$pegE$.ACTION_NAMES[[rule.id]]<-NULL
      pexSetAction(pegR, rule.id, action)
      pexSetActionInfo(pegR, rule.id, NULL)
    }
    else {
      stop("cannot set action: invalid action", call. = FALSE)
    }   
  } else {
    stop("cannot set action: invalid rule identifier", call. = FALSE)
  }
  invisible(TRUE)
}
