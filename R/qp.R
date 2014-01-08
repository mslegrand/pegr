#' A simple tool to examine a parsing expression in isolation
#' 
#' This can be quite useful in getting to know the PEG syntax
#' @param expression, a parsing expression (the right hand side of a rule definiton) 
#' @param arg, text string to apply expression to.
#' @param debugTree=TRUE, By default TRUE, so one can immediately graph (plot) or print (tree)
#' @return PEGResult, an object giving the result
#' @examples
#' # A simple choice operator
#' qp("'a' / 'b'", "ab")
#' # A simple sequence operator
#' qp("'a' 'b'", "ab")
#' # A combination of choice and sequence
#' qp("('a'/'c') ('b' / 'd')", "ab")
#' qp("('a'/'c') ('b' / 'd')", "cd")
#' # A lookahead not operator
#' qp("'a' !'b'", "ab" )
#' qp("'a' !'b'", "ac" )
#' # An lookahead and operator
#' qp("'a' & 'b'", "ab")
#' qp("'a' & 'b'", "ac")
#' # An optional operator
#' qp("'a' 'b'?", "ab")
#' qp("'a' 'b'?", "ac")
#' qp("'a'? 'b'", "ab")
#' qp("'a'? 'b'", "ba")
#' @export
qp<-function(expression, arg, debugTree=TRUE){
  peg<-new.parser()
  add_rule(peg, paste("R<-",expression))->res
  apply_rule(peg, 'R', input)->res
  res$Call$rule.id<-"Anonomous"
  return(res)
}
