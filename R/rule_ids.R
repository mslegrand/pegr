#' Lists all Rules contained in the parser
#' 
#' Returns a charactor vector giving all the defined rules contained
#' in the parser.
#' 
#' @examples
#' peg<-new.parser()
#' add_rule(peg, "I<-'Ice'")
#' add_rule(peg, "A<-'Age'")
#' add_rule(peg, "S<-'Scrat'")
#' rule_ids(peg) #c("I", "A", "S")
#' @param parser, a peg parser produced by  new.parser
#' @export
rule_ids<-function(pegr){
  if(!("pegR" %in% class(pegr))){ stop("argument not a peg parser")}
  ls(envir=pegr$pegE)->tmp
  if( any(grepl("atom.",tmp) ) ){
    tmp<-tmp[-grep("atom.",tmp)]    
  }
  tmp
}
