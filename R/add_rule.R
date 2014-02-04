
#' Adds a rule to the parser
#' 
#' Adds a rule to the parser, if the rule exists, it is overwritten, getting a new definition, 
#' a new (possibly empty) description, and a new (possibly empty) action.
#' 
#' @param parser, a peg parser produced by  new.parser
#' @param rule, the rule description, a quoted string that defines a rule according to the PEG Grammer
#' @param des, (optional: NULL by default) sets a rule description for this rule
#' @param act, (optional: NULL by default) sets an action, to be executed by this rule. This NULL by default.
#' @return Status and the rule processed
#' @seealso  For a description of
#' actions see \code{\link{set_action}} and \code{\link{appy_rule}} )
#' @examples
#' peg<-new.parser()
#' add_rule(peg, "X<-x", des="A bad rule (I forgot quotes)")
#' 
#' # Next we inspect the rule
#' inspect_rule(peg, "X") 
#' # Inspect rule returns the following
#' # Rule: X 
#' # Def: X<-x 
#' # Com: A bad rule (needs quotes) 
#' # Act: 
#' 
#' # Now we replace the rule by overwriting
#' add_rule(peg, "X<-'x'", act="list('X')")
#' # When again inspect, we see 
#' # the definition was fixed (x now is quoted), the description was removed, an action was added.
#' inspect_rule(peg,"X")
#' @export
add_rule<-function(parser, rule, des=NULL, act=NULL){
  if( !( "pegR" %in% class(parser) ) ){ stop("first argument not a parser", call. = FALSE) }
  if(!("character" %in% class(rule))){
    stop("second argument is not a character string", call. = FALSE)
  }
  pos<-badQuotePos(rule)
  if(pos>0){
    msg<-paste("Unbalance quotes at:", substr(rule, 1, pos))
    stop(msg, call. = FALSE)
  }
  res<-pexSetRule(parser, rule)
  #res<-parser$SET_RULE( rule )
  if(res$ok==TRUE){
    pexSetDescription(parser, res$rule.id, des) # direct call, no id checking
    set_action(parser, res$rule.id, act)
  }
  invisible( list(ok=res$ok, rule.id=res$rule.id, parsed=substr(rule,1,res$pos) ) )
}


#' Alternative to add_rule.
#' 
#' @param parser, a peg parser produced by  new.parser
#' @param arg, a list or vector specififyin a rule:
#' 
#' @details arg is a list or vector having 1-4 named components:
#' \itemize{
#' \item{ rule }{
#' (Mandatory) A string containing the peg rule definition
#' For example: \code{c(Rule="COLD<-'brrr'")}. The 'Rule' label is optional, but having a rule is mandatory.
#' }
#' \item{ des}{ 
#' (optional) A a textual string describing the rule. 
#'  For example: \code{c(Rule="COLD<-'brrr'", des="Polar" )} A comment must be named.
#'  }
#' \item{ act}{ 
#' (optional) an action specification. For example
#'  \code{c(Rule="COLD<-'brrr'", des="Polar", act=function(v){print("brr"); v} )}, An action must be named
#'  }
#' }
#' 
#' @examples
#' peg<- new.parser()
#' peg + "A<-'a'" + "B<-'b'" + "C<-'c'"
#' #to suppress the output use invisible"
#' invisible(peg + "A<-'a'" + "B<-'b'" + "C<-'c'")
#' #now add rule D with action and comment using a named character vector
#' peg + c("D<-'d'", des="capitalize D", act="list(atom='D')")
#' 
#' #now add rule E with action and comment a unnamed character
#' peg + c( "E<-'e'", "#double E", "{list('EE')}" )
#' 
#' @export
"+.pegR"<-function(parser, arg){
  #
  #use "{}" for act; use "#" for comment, use  =.. <- for rule
  if(is.null(arg)){
    stop("arg is null") #, call. = FALSE)
  } else {
    cmd<-preProcessArg(arg) #rule.id is null so no checking!
    #cmd<-arg2list(arg)
    if(is.null(cmd$rule)){
      stop("Missing Rule Definition")
    }
    res<-add_rule(parser, cmd$rule, des=cmd$des, act=cmd$act)
    if(res$parsed!=cmd$rule){
      delete_rule(parser, res$rule.id )
      stop(paste("Only Partially Processed! Stopped after:", res$rule.id ), call. = FALSE)
    }
  }
  invisible(parser)
}

arg2list<-function(arg){
  if( !("character" %in% class(arg)) & !( "list" %in% class(arg))){
    stop("NULL encountered", call. = FALSE)
  }
  if( is.null(names(arg))){
    nm<-"rule"
  } else {
    nm<-names(arg)
    nm[1]<-"rule"
  }
  arg<-as.list(arg)
  names(arg)<-nm
  arg
}


#quick kludge for checking for matching quotes
badQuotePos<-function(s){
  state<-0
  pos<-0
  for(i in 1:nchar(s)){
    if(substr(s,i,i)=='"' ){
      if(!(state==-1)){ #not in ' '
        state<-1-state #flip bits
        pos<-i
      }
    } else if(substr(s,i,i)=="'"){
      if(!(state==1)){
        state<--(1+state)
        pos<-i
      }
    }
  }
  return(pos*abs(state)) # pos if state=!0
}

