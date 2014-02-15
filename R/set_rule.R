#' Reset rule.definition in the parser
#' 
#' This only sets the rule definition (source).
#' This overwrites the old rule definition,leaving
#' rule actions and descriptions unaffected. To add rules see
#'\code{\link{add_rule}}
#'  
#' @param parser, a peg parser produced by  new.parser
#' @param rule, a quoted string that defines a rule according to the PEG Grammer
#' @return Status and the rule processed
#' @examples 
#' peg<-new.parser()
#' # Add rule A to recognize 'a' and return list('A') as it's value
#' add_rule(peg, "A<-'a'", act="list('A')")
#' value(apply_rule(peg, 'A', 'a', exe=TRUE))
#' set_definition(peg, "A<-'b'")
#' # Now A will only recognize 'b', so it will now fail on input 'a'
#' status(apply_rule(peg, 'A', 'a', exe=TRUE))
#' # However, even though  Arecognizes b, but the return value is still  list('A').
#' value(apply_rule(peg, 'A', 'b', exe=TRUE))
#' @export
set_definition<-function(parser, rule.id, rule.definition){
  if( !( "pegR" %in% class(parser) ) ){ stop("first argument not a parser\n", call. = FALSE) }
  if( !(rule.id) %in% pexGetIDs(parser) ){ stop("rule.id", rule.id, "not found\n")}
  if( !( "character" %in% class(rule.definition))){ stop("Third argument is not a rule\n", call. = FALSE)} 
  rule.definition<-validate.src(rule.definition)
  res<-pexSetRule(parser, rule.definition)
  invisible( list(ok=res$ok, parsed=substr(rule.definition,1,res$pos) ) )
}


#' Reset an existing rule: definition, description, action
#' 
#' Used to modify the rule \emph{definition} and/or \emph{description} and/or \emph{action} of of an exiting rule
#' 
#' @param parser, the peg parser containing the rule to be modified
#' @param rule.id, the identifier of the rule to be modified
#' @param value, the modifications to be applied. Can be either a named vector or a
#' named list (see below), a unnamed character vector(see below), or NULL, which case the rule is deleted. 
#' @return peg parser
#' 
#' @details
#' This is a very flexiable way of resetting the action, description and even the definition of an existing rule.
#' This encompasses the functionality of \code{\link{set_definition}}, \code{\link{set_description}} and \code{\link{set_action}} 
#' This rule accepts either \emph{named} vectors/lists or \emph{unnamed} vectors. In both cases, the vectors are character vectors
#' and the list components are either strings or a function representing an action. 
#' 
#' @section Using Named Vector:
#' For \bold{named vectors} the valid names are:
#' \itemize{
#' \item \emph{rule}: The name \emph{rule} is used to specify a field containing a \emph{rule definition}  (a.k.a. source). (like "A<-'a'") to be associated with the named rule.
#' \item \emph{des}: The name \emph{des} is used to specify a field containing a \emph{description} or comment to be associated with the named rule
#' \item  \emph{act}: The name \emph{act} is used to specify a field containing an \emph{action} to be associated with the rule. The action 
#' can either be NULL, or text which representing the body of function taking a single parameter v that is a list and returns a list.
#' When NULL, any existing action is deleted.
#' }
#' 
#' @section Using Unnamed Vectors:
#' For \bold{unnamed vectors}, each vector consists of a single string of the form:
#' \itemize{
#' \item \code{"...<-..."}: Specifies a \emph{rule definition}  (a.k.a. source). (like "A<-'a'")
#' \item \code{"#..."} Specifies \emph{description} or comment to be associated with the rule (like "# my clever rule")
#' \item  \code{"{...} "} Specifies an inline \emph{action} to be associated with the rule. (like "{list(toupper(unlist(v)))}" )
#' \item \code{"{}=NULL"} Specifies to delete any existing to be associated with the rule. 
#' }
#' 
#' @section Inline Action Short Cuts:
#' As a convenience the following short cuts are provided:
#' \itemize{
#' \item \code{"{}"}: Specifies the action \code{"{list()}"}, which essentially says drop (ignore) the values produce by this rule.
#' \item \code{"{-}"} Specifies the action \code{"list(paste(v,collapse=''))"}, which says paste all the values together.
#' }
#' 
#' 
#' @examples
#' # Modfiying a rule (adding a comment and  action):
#' peg<-new.parser()
#' peg + "A<-'a'"
#' peg
#' peg[["A"]]<-"A<-'ab'"
#' peg
#' peg[["A"]]<-c("A<-'xx'", des="replace xx by a", act="list('a')")
#' 
#' # Another way of doing the same thing (adding a comment and  action):
#' peg<-new.parser()
#' peg + "A<-'a'"
#' peg
#' peg[["A"]]<-"A<-'ab'"
#' peg
#' peg[["A"]]<-c("A<-'xx'", "#replace xx by a", "{list('a')}")
#' 
#' # The following are equivalent:
#' peg[['A']]<-list(act="list()")
#' peg[['A']]<-"{}" # A shortcut
#' 
#' # Deleteing a rule
#' peg<-new.parser()
#' peg + "A<-'a'" + "B<-'b'"
#' peg[["A"]]<-NULL
#' peg
#' 
#' 
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
    # delete_rule
    delete_rule(parser, rule.id)
    return(invisible(parser))
  }
  if( !("character" %in% class(arg)) & !( "list" %in% class(arg))){
    stop("Bad Rule assignment", call. = FALSE)
  }
  arg.L<-preProcessArg(arg,rule.id)
  #arg is now a list  
  if('rule' %in% names(arg.L)){
    pexSetRule(parser, arg.L$rule)
  }
  if('des' %in% names(arg.L)){
    #parser$SET_DESCRIPTION(rule.id, arg.L$des)
    if(arg.L$des!=""){
      pexSetDescription(parser, rule.id, arg.L$des)
    } else {
      pexSetDescription(parser, rule.id, NULL)
    }
    
  }
  if('act' %in% names(arg.L)){
    action<-arg.L$act
#     print(class(action))
#     print(body(action))
    set_action(parser, rule.id, action)
  }
  invisible(parser)
}

