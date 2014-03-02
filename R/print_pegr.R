#' Prints the rules contained in the PEG
#' 
#' @param peg parser produced by new.parser()
#' @examples
#' peg<-new.parser()
#' add_rule(peg, "NUM<-[0-9]+ (. [0-9] )?" )
#' peg<-set_description(peg, "NUM", "A Number")
#' peg<-add_rule(peg, "FT<-NUM ' '* ft")
#' peg<-set_description(peg, "FT"," measurement in feet")
#' # Now print the print the rules
#' print(peg)
#' @export
print.pegR<-function(parser){
  #list the rules in this peg
  if(!("pegR" %in% class(parser))){ stop("first argument not a peg parser", call. = FALSE)}  
  for(name in rule_ids(parser)){
    rs<-inspect_rule(parser, name)
    cat("\n")
    print(rs)
  }
}



#' A formatted printing for results of inspect_rule
#' 
#' See \code{\link{inspect_rule}}
#' @export
print.ruleStructure<-function(rs){
  cat(paste("Rule:",rs$name,"\n") )
  cat(paste("Def:", rs$def ,"\n") )
  cat(paste("Des:", ifelse(is.null(rs$com),"",rs$com),"\n") )
  cat(paste("Act:", ifelse(is.null(rs$act),"",paste(rs$act, collapse=" ") ),"\n") )
  invisible()
}

