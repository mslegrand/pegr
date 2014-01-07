#MAIN PEG GENERATOR
#' Parsing Made Bearable With Pegr
#' 
#' Pegr provides tools to parse using the parsing expression grammar (PEG) as defined
#' in Bryan Fords seminal work \href{http://www.brynosaurus.com/pub/lang/peg.pdf}{ParsingExpressionGrammars: A Recognition-Based Syntactic Foundation}. 
#' 
#' This implementation contains the following benefits
#' \enumerate{
#' \item Easy debugging of rules, since we can set any node to be the root
#' \item Printing a tree of all nodes visited during a parse, again helpful in debugging the rule set.
#' \item Plotting a tree of nodes visisted during a parse, again helpful in debugging
#' \item Providing a mechanism to add comments to nodes, just as we commonly add comments to code
#' }
#' 
#' @references ParsingExpressionGrammars: A Recognition-Based Syntactic Foundation -slides
#' \url{http://www.brynosaurus.com/pub/lang/peg-slides.pdf}
#' @import memoise
#' @docType package
#' @name pegr
NULL


#' @title add_rule
#' Adds a rule to the parser
#' 
#' @param parser, a peg parser produced by  new.parser
#' @param rule, a quoted string that defines a rule according to the PEG Grammer
#' @return Status and the rule processed
#' @export
add_rule<-function(parser, rule){
  if( !( "genE" %in% class(parser) ) ){ stop("first argument not a parser") }  
  res<-parser$DEFINITION(rule) 
  if(res$ok==TRUE){
    name<-strsplit(rule,"<-")[[1]][1]
    parser$pegE$.SOURCE.RULES[[name]]<-rule    
  } else {
    stop(paste("invalid syntax:",rule))
  }
  invisible( list(ok=res$ok, parsed=substr(rule,1,res$pos) ) )
}

#' Attach an action to the rule specified by rule.id
#' 
#' @param parser, a peg parser produced by  new.parser
#' @param rule.id, a character string naming the rule
#' @param action to be attached to the specified rule. The action may be
#' either a function acceptiong a list as input and a list as output or
#' a string of text which may be intrepted as a function body that returns 
#' a list
#' @export
set_action<-function(genE, rule.id, action){
  #TODO:  ( expression?)
  #TODO: refactor using switch?
  if(!("genE" %in% class(genE))){ stop("first argument not a peg parser")}  
  if( rule.id %in% rule_ids(genE)){
    if(class(action)=="character"){
      action<-paste("function(v){",action,"}")
      genE$pegE$.ACTION[[rule.id]]<-eval(parse(text=action))  
      return(TRUE)
    } else if (class(action)=="function"){
      genE$pegE$.ACTION[[rule.id]]<-action       
      return(TRUE)
    }
    stop("cannot set action: invalid action")
    return(FALSE)
  } else {
    stop("cannot set action: invalid rule identifier")
  }
}

#' Attaches an (optional) description to the given rule.
#' 
#' A description
#' should be used to comment a given rule
#' 
#' @param parser, a peg parser produced by  new.parser
#' @param rule.id, a character string naming the rule
#' @param description, a text string describing the rule
#' @export
set_description<-function(genE, rule.id, description){
  if(!("genE" %in% class(genE))){ stop("first argument not a peg parser")}  
  if( rule.id %in% rule_ids(genE)){
    genE$pegE$.RULE_DESCRIPT[[rule.id]]<-description
    invisible(TRUE)
  } else {
    stop("cannot add description: invalid rule identifier")
  }
}

#' Gets a description of a given rule
#' 
#' @param rule.id, a character string naming the rule
#' @param parser, a peg parser produced by  new.parser
#' @return description, a character string describing the parser
#' @export
get_description<-function(genE, rule.id){
  if(!("genE" %in% class(genE))){ stop("first argument not a peg parser")}  
  if( rule.id %in% rule_ids(genE)){
    description<-genE$pegE$.RULE_DESCRIPT[[rule.id]]
    return(description)
  } else {
    stop("cannot add description: invalid rule identifier")
  }
  
}

#' Deletes the given rule form the parser.
#' 
#' @param parser, a peg parser produced by  new.parser
#' @param rule.id, a character string naming the rule
#' @export
delete_rule<-function(genE, rule.id){
  #delete rule 
  if(!("genE" %in% class(genE))){ stop("first argument not a peg parser")}  
  genE$pegE$.SOURCE.RULES[[rule.id]]<-NULL
  genE$pegE$.ACTION[[rule.id]]<-NULL
  genE$pegE$.RULE_DESCRIPT[[rule.id]]<-NULL
  rm(list=rule.id, envir=genE$pegE)    
}

#' Lists all Rules contained in the parser
#' @param parser, a peg parser produced by  new.parser
#' @export
rule_ids<-function(genE){
  if(!("genE" %in% class(genE))){ stop("argument not a peg parser")}
  ls(envir=genE$pegE)->tmp
  if( any(grepl("atom.",tmp) ) ){
    tmp<-tmp[-grep("atom.",tmp)]    
  }
  tmp
}

#' Gets the inner environment of the parser
#' @export
get_pegE<-function(genE){
  if(!("genE" %in% class(genE))){ stop("first argument not a generator")}  
  genE$pegE
}

#' Invoke the parserto parse using the rule.id as the root.
#' 
#' @param parser, a peg parser produced by  new.parser
#' @param rule.id, a character string naming the rule
#' @param arg, a character string to be parsed
#' @param exe, a flag indicate whether actions should be performed. 
#' when false, no actions will be executed
#' @param debugTree, a flag which when set produces tree of snapshots of all nodes
#' visited during the parsing process
#' @export
apply_rule<-function(parser, rule.id, arg, exe=FALSE, debugTree=FALSE){
  if(!("genE" %in% class(parser))){ stop("first argument not a peg parser")}  
  if( !( rule.id %in% rule_ids(parser) ) ){stop("cannot parse: invalid rule identifier")}
  parser$pegE$.DEBUG.NODE<-debugTree
  parser$pegE[[rule.id]](arg, exe)->res
  if(!"list" %in% (class(res)) ){ stop("Bad Action Rule: resulting value is not a list")}
  #parserName<-as.character( substitute(parser) )
  #res$Call<-list(parserName<-parserName, rule.id=rule.id, arg=arg)
  res$Call<-list(rule.id=rule.id, arg=arg)
  res$options<-list(exe=exe, debugTree=debugTree)
  class(res)<-c("PEGResult")
  res
}

#' Prints the parser container
#' @export
print.pegE<-function(pegE){
  #list the rules in this peg
  ls(envir=pegE)
}


#' Summarizes a parsing result
#' @export
summary.PEGResult<-function(res){
  if(!("PEGResult" %in% class(res))){ stop("Argument not a peg parsing result")}
  s<-list(
  Call=paste0(res$Call$rule.id, "(" ,res$Call$arg, ")" ),
  Options=paste("Options:", "Apply Actions=",res$options$exe,"Make Tree=",res$options$debugTree,""),
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


#' Returns the value of a parsing  result
#' 
#' @return A list containing the values computed
#' @export
value<-function(res){
  if(!("PEGResult" %in% class(res))){ stop("Argument not a peg parsing result")} 
  return(res$val) 
}


#todo:
#turn on all memoize
#print history of run 
#print peg(parse) history #history(pegE.result)
#print peg(parse) final state 
#print rule name/ description describe(pegE)
#print all rules: rules(pegE)
#add rule objects for printing, applying etc.
#summary(pegE)

