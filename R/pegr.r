
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


#' Attach an (optional) description to a specified rule
#' 
#' A description
#' should be used to comment a given rule
#' 
#' @param parser, a peg parser produced by  new.parser
#' @param rule.id, a character string naming the rule
#' @param description, a text string describing the rule
#' @export
set_description<-function(pegR, rule.id, description){
  if(!("pegR" %in% class(pegR))){ stop("first argument not a peg parser", call. = FALSE)}  
  if( rule.id %in% rule_ids(pegR)){
    #pegR$SET_DESCRIPTION(rule.id, description)
    pexSetDescription(pegR, rule.id, description)
    invisible(TRUE)
  } else {
    stop("cannot set description: invalid rule identifier", call. = FALSE)
  }
}

#' Gets a description of a given rule
#' 
#' Descriptions are like comments for rules, it is a good practice to comment 
#' your rules after you add them. See \code{\link{set_description}}
#' 
#' @param rule.id, a character string naming the rule
#' @param parser, a peg parser produced by  new.parser
#' @return description, a character string describing the parser
#' @export
get_description<-function(pegR, rule.id){
  if(!("pegR" %in% class(pegR))){ stop("first argument not a peg parser", call. = FALSE)}  
  if( rule.id %in% rule_ids(pegR)){
    #description<-pegR$GET_DESCRIPTION( rule.id)
    description<-pexGetDescription(pegR, rule.id)
    return(description)
  } else {
    stop("cannot get description: invalid rule identifier", call. = FALSE)
  }  
}

#' Deletes the given rule form the parser.
#' 
#' @param parser, a peg parser produced by  new.parser
#' @param rule.id, a character string naming the rule
#' @export
delete_rule<-function(pegR, rule.id){
  #delete rule 
  if(!("pegR" %in% class(pegR))){ stop("first argument not a peg parser", call. = FALSE)}  
  pegR$pegE$.SOURCE.RULES[[rule.id]]<-NULL
  pegR$pegE$.ACTION[[rule.id]]<-NULL
  pegR$pegE$.RULE_DESCRIPT[[rule.id]]<-NULL
  pegR$pegE$.ACTION_NAMES[[rule.id]]<-NULL
  rm(list=rule.id, envir=pegR$pegE)    
}

rule_source<-function(parser, rule.id){
  parser$pegE$.SOURCE.RULES[[rule.id]]
}


#' Prints the rules contained in the PEG
#' 
#' @param peg parser produced by new.parser()
#' @examples
#' peg<-new.parser()
#' add_rule(peg, "NUM<-[0-9]+ (. [0-9] )?" )
#' set_description(peg, "NUM", "A Number")
#' add_rule(peg, "FT<-NUM ' '* ft")
#' set_description(peg, "FT"," measurement in feet")
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
  cat(paste("Com:", ifelse(is.null(rs$com),"",rs$com),"\n") )
  cat(paste("Act:", ifelse(is.null(rs$act),"",paste(rs$act, collapse=" ") ),"\n") )
  invisible()
}








#' Summarizes a parsing result
#' @export
summary.PEGResult<-function(res){
  if(!("PEGResult" %in% class(res))){ stop("Argument not a peg parsing result", call. = FALSE)}
  s<-list(
  Call=paste0(res$Call$rule.id, "(" ,res$Call$arg, ")" ),
  Options=paste("Options:", "Apply Actions=",res$options$exe,"Make Tree=",res$options$record,""),
  Status<-paste("Status:", ifelse(res$ok,"Success","Failure"),"")
  )
  if(res$ok){
    s$Processed<-paste("Processed:", res$pos, "out of", length(res$arg), "")
    val<-paste(res$val,collapse=",")
    s$val<-paste("Evaluates to: list(", val, ")\n")
  }
  class(s)<-"summary.PEGResult"
}

print.summary.PEGResult<-function(sum){
  cat(paste(sum,collapse="\n"))
}

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

#todo:
#turn on all memoize


#' Turn on the pegR rule debugger
#' 
#' The pegR rule debugger allows one to step through as the rules
#' as they are invoked.
#' 
#' 
#'  When applying a given rule to to a text input string,  
#'  that rule may call other rules which in turn may call other 
#'  rules, ad nausem. The  peg rule debugger allows one to 
#'  step through that calling sequence, examining the state
#'  of the parsing both upon entering a rule and 
#'  and exiting a rule. 
#'  @examples
#'  \dontrun{
#'  pegR<-new.parser()
#'  debug.pegR(pegR)
#'  pegR + "A<-B" + "B<-C" + "C<-'abc'"
#'  pegR[['A']]('abc')
#'  undebug.pegR(pegR)
#'  }

#' @seealso \code{\link{undebug.pegR}}
#' @export
debug.pegR<-function(peg){
  pexSetDebugOn(peg, TRUE)
}


#' Turn off the pegR rule debugger
#' 
#' @seealso \code{\link{debug.pegR}}
#' 
#' 
#' @export
undebug.pegR<-function(peg){
  pexSetDebugOn(peg, FALSE)    
}
  