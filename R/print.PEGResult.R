#' prints the final result of applying a rule to text input
#' 
#' @examples
#'   peg<-new.parser()
#'   add_rule(peg, "A<-'a'")
#'   add_rule(peg, "B<-'b'") 
#'   add_rule(peg, "D<-'d'")
#'   add_rule(peg, "C<-'c'")
#'   add_rule(peg,"ROOT<-A B C D")
#'   apply_rule(peg,"ROOT","abcd")->res
#'   res #invokes print.PEGResult
#'   @note for readability, quotes are left out when printing the list.
#' @export
print.PEGResult<-function(res){
  if(!("PEGResult" %in% class(res))){ stop("Argument not a peg parsing result")} 
  odt<-res$options$record
  oex<-res$options$exe
  tmp<-c("; Options: ",": record=", res$options$record, ": exe=", res$options$exe)
  tmq<-c(oex| odt, odt, odt, oex, oex)
  opts=ifelse( oex | odt, tmp[tmq], "")
  cat(paste0("Call: Rule=", paste0(  res$Call$rule.id, "; Input Arg=\"",res$Call$arg,"\" ",opts, "\n")  ))
  #cat(paste0("Call: (", paste0( res$Call$parserName,",", res$Call$rule.id, ",",res$Call$arg,",record=",res$options$record,",exe=",res$options$exe,")\n")  ))
  #cat(paste("Options:", "Apply Actions=",res$options$exe,"Make Tree=",res$options$record,"\n"))
  cat(paste("Status:", res$ok, "\n") ) 
  cat(paste("Consumed: (", res$pos , "/", nchar(res$Call$arg) ,")\n" ))
  if(res$ok==TRUE){
    val<-paste(res$val,collapse=",")
    cat(paste("Evaluates to: list(", val, ")\n"))
  } else {
    cat("Status: Failure\n")
  }
  invisible(res)
}
