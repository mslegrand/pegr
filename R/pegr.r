
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
#' @docType package
#' @name pegr
NULL


#' Attach an (optional) action to a specified rule
#' 
#' @param parser, a peg parser produced by  new.parser
#' @param rule.id, a character string naming the rule
#' @param action to be attached to the specified rule. The action may be
#' may take three forms:
#' \enumerate{
#'  \item  a function accepting a list as input and a list as output
#'  \item  a string of text to be interpreted as a function body with an input parameter consisting of a
#'  list named v, and return value which is also a list.
#'  \item  NULL, in which case the action associated with the given rule is removed.
#' }
#' @examples
#' #Capitalize all occurances of t using function calls
#' peg<-new.parser()
#' add_rule(peg, "T<-'t'")
#' add_rule(peg, "R<-(T / .)+")
#' f<-function(v){return(list('T'))}
#' set_action(peg, "T", f)
#' g<-function(v){return(list(paste(v,collapse='')))}
#' set_action(peg, "R", g )
#' value(apply_rule(peg, "R", "cat in the hat", exe=T))
#' 
#' #Capitalize all occurances of A using inline actions
#' peg<-new.parser()
#' add_rule(peg, "A<-'a'")
#' add_rule(peg, "R<-(A / .)+")
#' set_action(peg, "A", "list('A')")
#' set_action(peg, "R", "list(paste(v, collapse=''))" )
#' value(apply_rule(peg, "R", "cat in the hat", exe=T))
#' 
#' @export
set_action<-function(pegR, rule.id, action){
  #TODO:  ( expression?)
  #TODO: refactor using switch?
  if(!("pegR" %in% class(pegR))){ stop("first argument not a peg parser")}  
  if( rule.id %in% rule_ids(pegR)){
    if(class(action)=="character"){
      pegR$pegE$.ACTION_NAMES[[rule.id]]<-c("Inline:",action)
      action<-paste("function(v){",action,"}")
      pegR$pegE$.ACTION[[rule.id]]<-eval(parse(text=action))  
    } else if (class(action)=="function"){
      pegR$pegE$.ACTION[[rule.id]]<-action 
      pegR$pegE$.ACTION_NAMES[[rule.id]]<-c("External Function:", deparse(substitute(action)))
    } else if (is.null(action)){
      pegR$pegE$.ACTION[[rule.id]]<-action
      pegR$pegE$.ACTION_NAMES[[rule.id]]<-NULL
    }
    else {
      stop("cannot set action: invalid action")
    }   
  } else {
    stop("cannot set action: invalid rule identifier")
  }
  invisible(TRUE)
}

#' Retrieve an action to the rule specified by rule.id
#' 
#' @param parser, a peg parser produced by  new.parser
#' @param rule.id, a character string naming the rule
#' @return action attached to the specified rule. The action may be
#' may take two forms:
#' \enumerate{
#'  \item the name of a function
#'  \item  a string of text interpreted as a function body with an input parameter consisting of a
#'  list named v, and return value which is also a list.
#' }
#' @examples
#' # Delete all vowels
#' peg<-new.parser()
#' add_rule(peg, "V<-'a' / 'e' / 'i' / 'o' / 'u' ")
#' add_rule(peg, "R<-(V / .)+")
#' set_action(peg, "V", "list()" )
#' g<-function(v){ list(paste(v,collapse='')) }
#' set_action(peg, "R", g )
#' #see the result
#' value(apply_rule(peg, "R", "cat in the hat", exe=T))
#' # inspect the action for rule "V"
#' get_action(peg, "V")
#' # inspect the action for rule "R"
#' get_action(peg, "R")
#' @export
get_action<-function(pegR, rule.id){
  if(!("pegR" %in% class(pegR))){ stop("first argument not a peg parser")}  
  if( rule.id %in% rule_ids(pegR)){
    actionTxt<-pegR$pegE$.ACTION_NAMES[[rule.id]]   
  } else {
    stop("cannot get action: invalid rule identifier")
  }
  actionTxt
}

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
  if(!("pegR" %in% class(pegR))){ stop("first argument not a peg parser")}  
  if( rule.id %in% rule_ids(pegR)){
    pegR$pegE$.RULE_DESCRIPT[[rule.id]]<-description
    invisible(TRUE)
  } else {
    stop("cannot set description: invalid rule identifier")
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
  if(!("pegR" %in% class(pegR))){ stop("first argument not a peg parser")}  
  if( rule.id %in% rule_ids(pegR)){
    description<-pegR$pegE$.RULE_DESCRIPT[[rule.id]]
    return(description)
  } else {
    stop("cannot get description: invalid rule identifier")
  }  
}

#' Deletes the given rule form the parser.
#' 
#' @param parser, a peg parser produced by  new.parser
#' @param rule.id, a character string naming the rule
#' @export
delete_rule<-function(pegR, rule.id){
  #delete rule 
  if(!("pegR" %in% class(pegR))){ stop("first argument not a peg parser")}  
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
  if(!("pegR" %in% class(parser))){ stop("first argument not a peg parser")}  
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
print.ruleStruct<-function(rs){
  cat(paste("Rule:",rs$name,"\n") )
  cat(paste("Def:", rs$def ,"\n") )
  cat(paste("Com:", ifelse(is.null(rs$com),"",rs$com),"\n") )
  cat(paste("Act:", ifelse(is.null(rs$act),"",paste(rs$act, collapse=" ") ),"\n") )
  invisible()
}

 #' Inspects a given rule contained within a parser
 #' 
 #' This is used to see what a comprises a given rule. 
 #' @param parser, a peg parser
 #' @param rule_id, a rule idenitifier, a.k.a. rule name
 #' @return ruleStruct, a container which when printed will produce a
 #' a summary of that rule
 #' 
 #' @examples
 #' peg<-new.parser()
 #' add_rule(peg, "DOG<-'fido' / 'spot' / 'rover'/ 'buddy'")
 #' set_action(peg, "DOG", "list('bark')")
 #' set_description(peg, "DOG", "sound of dog")
 #' inspect_rule(peg, "DOG")
 #' @export
inspect_rule<-function(parser, rule_id){
  if( !( "pegR" %in% class(parser) ) ){ stop("first argument not a parser") }    
  name<-rule_id #name
  def<-rule_source(parser, rule_id) 
  com<-get_description(parser, rule_id)
  act<-get_action(parser, rule_id)
  ruleStruct(name, def, com, act)
}


ruleStruct<-function(name, def, descript=NULL, action=NULL){
  rs<-list(name=name, def=def, com=descript, act=action )
  class(rs)<-"ruleStruct"
  rs
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
#print history of run 
#print peg(parse) history #history(pegE.result)
#print peg(parse) final state 
#print rule name/ description describe(pegE)
#print all rules: rules(pegE)
#add rule objects for printing, applying etc.
#summary(pegE)

