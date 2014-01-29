
#' Parsing Made Bearable With Pegr
#' 
#' Pegr provides tools to parse using the parsing expression grammar (PEG) as defined
#' in Bryan Fords seminal work \href{http://www.brynosaurus.com/pub/lang/peg.pdf}{ParsingExpressionGrammars: A Recognition-Based Syntactic Foundation}. 
#' 
#' This implementation contains the following benefits
#' \enumerate{
#' \item Easy debugging of rules, since we can set any node to be the root
#' \item A tool (qp) to quickly parse simple expressions on the fly, great for learning PEG or confirming those constructs.
#' \item Printing a tree of all nodes visited during a parse, again helpful in debugging the rule set.
#' \item Plotting a tree of nodes visisted during a parse, again helpful in debugging
#' \item Providing a mechanism to add comments to nodes, just as we commonly add comments to code
#' }
#' 
#' @references ParsingExpressionGrammars: A Recognition-Based Syntactic Foundation -slides
#' \url{http://www.brynosaurus.com/pub/lang/peg-slides.pdf}
#' @import memoise
#' @import stringr
#' @docType package
#' @name pegr
NULL



#' A convenient shortcut for pasting lists
#' 
#' This is equivalent to list(paste(v,collapse=''). It is analogous to pastes0 for
#' vectors.
#' @param v, a list
#' @references a list of length one, having its sole element
#' the result of pasting together the contents of v without any
#' seperator
#' @export
paste1<-function(v){ list(paste(v,collapse='')) }





#' @export
"[.pegR"<-function(parser, rule.id){
  stop("pegR is not subsettable", call.=FALSE)
}

#' @export
"[<-.pegR"<-function(parser, rule.id){
  stop("pegR is not subsettable", call.=FALSE)
}

"[.pegR"<-function(parser, rule.id, value){
  stop("pegR is not subsettable", call.=FALSE)
}  
  