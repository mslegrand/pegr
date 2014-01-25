
#' get data.frame consisting of peg rules
#' 
#' @param parser, a peg parser produced by  new.parser
#' @return data.frame of rules
#' 
#' @examples
#' peg<-new.parser()
#' peg + 
#'  c("A<-'a'", des="a 2 A", action="'A'") +
#'  c("B<-'b'", des="b does nothtng")
#' as.data.frame(peg)
#' @export
as.data.frame.pegR<-function(peg, ...){
  pexGetRulesAsDataFrame(peg, ...)
}
