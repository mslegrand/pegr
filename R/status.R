#' Returns the Status of a parsing  result
#' 
#' @param A result from parsing, i.e. the result form applying a rule to a text string input.
#' @return TRUE if successful, FALSE otherwise
#
#' @examples 
#' parser<-new.parser()
#' #This rule to tests for string of a's followed by an equal number of b's
#' peg<-add_rule(parser, "S<-'a' S 'b'") 
#' 
#' #test against 3 a's followed b 3 b's returns TRUE
#' apply_rule(parser, 'S', 'aaaabbbb')
#' status(res)
#' 
#' #test again with input of 3 a's followed bu 3 b's, 
#' #this returns False
#' res<-apply_rule(parser, 'S', 'aaabbbb')
#' status(res)
#' 
#' See also 
#' @export
status<-function(res){
  if(!("PEGResult" %in% class(res))){ stop("Argument not a peg parsing result", call. = FALSE)} 
  res$ok
}
