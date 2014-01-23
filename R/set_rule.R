#' Sets a rule in the parser
#' 
#' This only sets the rule and definition.
#' Thus for existing rules, this overwrites the rule definition, but leaves 
#' rule actions and descriptions unaffected. For non-existing rules this is equivalent
#' to add_rule(parser, rule, descrip=NULL, action=NULL) See \code{\link{add_rule}}
#'  
#' @param parser, a peg parser produced by  new.parser
#' @param rule, a quoted string that defines a rule according to the PEG Grammer
#' @return Status and the rule processed
#' @examples 
#' peg<-new.parser()
#' # Add rule A to recognize 'a' and return list('A') as it's value
#' add_rule(peg, "A<-'a'", act="list('A')")
#' value(apply_rule(peg, 'A', 'a', exe=TRUE))
#' set_rule(peg, "A<-'b'")
#' # Now A will only recognize 'b', so it will now fail on input 'a'
#' status(apply_rule(peg, 'A', 'a', exe=TRUE))
#' # However, A not recognizes b, but still returs list('A') as it's value
#' value(apply_rule(peg, 'A', 'b', exe=TRUE))
#' @export
set_rule<-function(parser, rule){
  if( !( "pegR" %in% class(parser) ) ){ stop("first argument not a parser", call. = FALSE) }  
  if( !( "character" %in% class(rule))){ stop("Second argument is not a rule", call. = FALSE)}
  #add check for existing rule
  #res<-parser$DEFINITION(rule) 
#   res<-parser$DEFINITION(parser$pegE, rule) 
#   if(res$ok==TRUE){
#     name<-strsplit(rule,"<-")[[1]][1]
#     name <- gsub("^\\s+|\\s+$", "", name)
#     parser$pegE$.SOURCE.RULES[[name]]<-rule   
#   } else {
#     stop(paste("invalid syntax:",rule))
#   }
  #res<-parser$SET_RULE(parser$pegE, rule)
  res<-pexSetRule(parser, rule)
  invisible( list(ok=res$ok, parsed=substr(rule,1,res$pos) ) )
}

#"[<-.ar"<-function(u,v, value)

#' Reset an existing rule
#' 
#' Used to Modify the rule definition and or description or value of of an exiting rule
#' 
#' @param parser, the peg parser containing the rule to be modified
#' @param rule.id, the identifier of the rule to be modified
#' @param value, the modifications to be applied. Can be either a vector or a
#' list. 
#' @return peg parser
#' @examples
#' peg<-new.parser()
#' peg + "A<-'a'"
#' peg
#' peg[["A"]]<-"A<-'ab'"
#' peg
#' peg[["A"]]<-c("A<-'xx'", des="replace xx by a", act="list('a')")
#' peg
#' @export
"[[<-.pegR"<-function(parser, rule.id, value){
  # first parse arg (which will be some string or list) 
   #cat("hello/n")
   arg<-value
  if( is.null(rule.id) | !(rule.id %in% rule_ids(parser) )){
    stop("invalid rule id", call. = FALSE)
  }
  if( length(rule.id)!=1 ){
    stop("The index must contain exactly one rule", call. = FALSE)
  }
  if( is.null(arg) ) {
    stop("NULL assignment not yet implemented") #TODO!!! inplement as delete
  }
  if( !("character" %in% class(arg)) & !( "list" %in% class(arg))){
    stop("Bad Rule assignment", call. = FALSE)
  }
  if( is.null(names(arg) ) ){
    nR<-"rule"
  } else {
    nR<-names(arg)
    # substite "rule" for the name of the first unnamed arg
    if(any(nR=="")){
      nR[min(which(nR==""))]<-"rule"
    }    
  }
  arg.L<-as.list(arg)
  names(arg.L)<-nR
  #arg is now a list
  
  if('rule' %in% names(arg.L)){
    #we need to check that rule.id is correct! We extract from arg.rule
    rule<-arg.L$rule
    name<-strsplit(rule,"<-")[[1]][1]
    name <- gsub("^\\s+|\\s+$", "", name)
    if(name!=rule.id){
      stop("Rule does not match rule.id!", call. = FALSE)
    }
    #set_rule(parser, arg.L$rule)
    pexSetRule(parser, rule)
  }
  if('des' %in% names(arg.L)){
    #parser$SET_DESCRIPTION(rule.id, arg.L$des)
    pexSetDescription(parser, rule.id, arg.L$des)
  }
  if('act' %in% names(arg.L)){
    set_action(parser, rule.id, arg.L$act)
  }
  invisible(parser)
}
