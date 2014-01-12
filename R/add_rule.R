
#' Adds a rule to the parser
#' 
#' Adds a rule to the parser, if the rule exists, it is overwritten, getting a new definition, 
#' a new (possibly empty) description, and a new (possibly empty) action.
#' 
#' @param parser, a peg parser produced by  new.parser
#' @param rule, a quoted string that defines a rule according to the PEG Grammer
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
  if( !( "genE" %in% class(parser) ) ){ stop("first argument not a parser") }
  if(!("character" %in% class(rule))){
    stop("second argument is not a character string")
  }
  pos<-badQuotePos(rule)
  if(pos>0){
    msg<-paste("Unbalance quotes at:", substr(rule, 1, pos))
    stop(msg)
  }
  res<-parser$DEFINITION(rule) 
  if(res$ok==TRUE){
    name<-strsplit(rule,"<-")[[1]][1]
    name <- gsub("^\\s+|\\s+$", "", name)
    parser$pegE$.SOURCE.RULES[[name]]<-rule   
    set_description(parser, name, des)
    set_action(parser, name, act)
  } else {
    stop(paste("invalid syntax:",rule))
  }
  invisible( list(ok=res$ok, rule.id=name, parsed=substr(rule,1,res$pos) ) )
}

#' Alternative to add_rule.
#' 
#' @examples
#' peg<- new.parser()
#' peg + "A<-'a'" + "B<-'b'" + "C<-'c'"
#' #to suppress the output use invisible"
#' invisible(peg + "A<-'a'" + "B<-'b'" + "C<-'c'")
#' #now add rule D with action and comment
#' peg + c("D<-'d'", des="capitalize D", act="list(atom='D')")
#' @export
"+.genE"<-function(parser, arg){
  #
  #use "{}" for act; use "#" for comment, use  =.. <- for rule
  if(is.null(arg)){
    stop("arg is null")
  } else {
    cmd<-arg2list(arg)
    res<-add_rule(parser, cmd$rule, des=cmd$des, act=cmd$act)
    if(res$parsed!=cmd$rule){
      delete_rule(parser, res$rule.id )
      stop(paste("Partial Processed:", rule, ))
    }
  }
  return(parser)
}

arg2list<-function(arg){
  if( !("character" %in% class(arg)) & !( "list" %in% class(arg))){
    stop("NULL encountered")
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

