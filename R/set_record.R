#' Sets the peg to recording mode
#' 
#' When recording is on (TRUE),  applying a peg rule to input a record of  
#'  the tree of nodes (and their values) is saved for use by
#' \code{\link{tree}} or \code{\link{plot}}. This value is 
#' a default value and can be override by setting the record option during the rule application 
#' 
#' 
#' @param peg parser produced by new.parser()
#' @param on, when set to TRUE, turns on the recording status.
#' 
#' @seealso \code{\link{tree}} \code{\link{plot}} \code{\link{apply_rule}}
#' @export
set_record_model<-function(peg, on=TRUE){
  pexSetRecordDefault(peg, on)
}
