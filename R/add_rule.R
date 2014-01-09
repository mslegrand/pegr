#' @title add_rule
#' Adds a rule to the parser
#' 
#' Adds a rule to the parser, if the rule exists, it is overwritten, getting a new definition, 
#' a new (possibly empty) description, and a new (possibly empty) action.
#' 
#' @param parser, a peg parser produced by  new.parser
#' @param rule, a quoted string that defines a rule according to the PEG Grammer
#' @param des, (optional: NULL by default) sets a rule description for this rule
#' @param act, (optional: NULL by default) sets an action, to be executed by this rule. For a description of
#' actions see \code{\link{set_action}} and \code{\link{appy_rule}} ) This NULL by default.
#' @return Status and the rule processed
#' 
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
  if( !( "genE" %in% class(parser) ) ){ stop("first argument not a parser") }  
  res<-parser$DEFINITION(rule) 
  if(res$ok==TRUE){
    name<-strsplit(rule,"<-")[[1]][1]
    parser$pegE$.SOURCE.RULES[[name]]<-rule   
    set_description(parser, name, des)
    set_action(parser, name, act)
  } else {
    stop(paste("invalid syntax:",rule))
  }
  invisible( list(ok=res$ok, parsed=substr(rule,1,res$pos) ) )
}