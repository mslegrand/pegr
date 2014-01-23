#' Sets  restriction on the maximum call depth of rules
#' 
#' @param parser, a peg parser produced by  new.parser
#' @param stop.level.limit, a restriction on the number of calls (levels) before stopping a parse
#' 
#' @details  Since a rule can call other rules or even itself, the depth of a calling sequence
#' can grow and potentially be infinited. By setting the stop level one is restricting that depth,
#' and hence can detect possible infinite recursive calls. To inspect the calling sequence of those
#' rules when the max depth of the rule stack is exceed, use \code{\link{get_rule_stack}}
#' @examples
#' \dontrun{
#' peg<-new.parser()
#' set_rule_stack_limit(peg, 10)
#' add_rule(peg, "A<-A" ) #an infinite recursive call
#' apply_rule(peg, "A", "x") #the input is irrevelant, throws error
#' }
#' @seealso \code{\link{get_rule_stack}}, \code{\link{unset_rule_stack_limit}}
#' @export
set_rule_stack_limit<-function(pegR, stop.level.limit){
  pexSetStopLevel(pegR, stop.level.limit)
  invisible(NULL)
}

#' Unsets a restriction the maximum lcall depth of rules
#' 
#' Unsets the retriction that was set by \code{\link{set_rule_stack_limit}}
#' @param parser, a peg parser produced by  new.parser
#' 
#' @examples
#' \dontrun{
#' peg<-new.parser()
#' set_rule_stack_limit(peg, 10)
#' add_rule(peg, "A<-'a' A / ''" ) #consumes all a's at the beginning of a string
#' apply_rule(peg, "A", "aaaaaaaaaaaax") #there are 12 a's but will stop at 10
#' #error, rule stack depth limit exceeded
#' unset_rule_stack_limit(peg)
#'apply_rule(peg, "A", "aaaaaaaaaaaax") #now succeeds
#'}
#' @seealso \code{\link{get_rule_stack}}, \code{\link{set_rule_stack_limit}}
#' @export
unset_rule_stack_limit<-function(pegR){
  pexUnSetStopLevel(pegR)
  invisible(NULL)
}

#' Returns the call stack when the stop level was exceeded
#' 
#' @param parser, a peg parser produced by  new.parser
#' @return call stack, a data frame show the sequence of calls encounterd
#' prior to the exceeding the stop level
#' 
#' @examples
#' \dontrun{
#' peg<-new.parser()
#' set_rule_stack_limit(peg, 20)
#' add_rule(peg, "A<-B" ) 
#' add_rule(peg, "B<-C")
#' add_rule(peg, "C<-A") # A vicious circle of calls resulting in infinite recursion
#' apply_rule(peg, "A", "x") # will stop at 20
#' get_rule_stack(peg)
#' }
#' @export
get_rule_stack<-function(pegR){
  pexGetStack(pegR)
}
