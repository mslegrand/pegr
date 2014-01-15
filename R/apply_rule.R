

#' Applies the rule to the specified text
#' 
#' That is invoke the parser to parse a given input text using 
#' the specified rule as the root. 
#' 
#' @param parser, a peg parser produced by  new.parser
#' @param rule.id, a character string naming the rule
#' @param arg, a character string to be parsed
#' @param exe, (optional, default=FALSE) a flag indicate whether actions should be performed. 
#' when FALSE, no actions will be executed
#' @param debugTree, (optional, default=FALSE) a flag which when set produces tree of snapshots of all nodes
#' visited during a successful parse.
#' @return A PEGResult, a container containing the status of the parsing attempt, a record of the characters parsed, 
#' and a list of one or more values computed during the parse. If exe not set (FALSE) or no actions have been attached
#' to the rules, then the values will simply be a list of all literals (characters, or strings) that appeared in the 
#' input text.
#' @examples
#' # The simplest example: a parser that only accepts a single character 'a'
#' peg<-new.parser()
#' add_rule(peg, "A<-'a'")
#' res<-apply_rule(peg, 'A' "a")
#' res # the results
#' # A more complex example: 
#' # A number parser: Extracts number
#' peg<-new.parser()
#' add_rule(peg, "NUM<-[0-9]+ ('.' [0-9]*)?)", act = "paste1(v)", des = "numbers")
#' apply_rule(peg, "NUM", "12.3", exe=T)
#' 
#' @example
#' demo/distConv.R
#' @export
apply_rule<-function(parser, rule.id, input.text, exe=FALSE, debugTree=FALSE){
  if(!("genE" %in% class(parser))){ stop("first argument not a peg parser")}  
  if( !( rule.id %in% rule_ids(parser) ) ){stop("cannot parse: invalid rule identifier")}
  parser$pegE$.DEBUG.NODE<-debugTree
  parser$pegE[[rule.id]](input.text, exe)->res
  if(!"list" %in% (class(res)) ){ stop("Bad Action Rule: resulting value is not a list")}
  #parserName<-as.character( substitute(parser) )
  #res$Call<-list(parserName<-parserName, rule.id=rule.id, arg=input.text)
  res$Call<-list(rule.id=rule.id, arg=input.text)
  res$options<-list(exe=exe, debugTree=debugTree)
  class(res)<-c("PEGResult")
  res
}




#' An alternative to apply_rule
#' 
#' 
#' @examples
#' peg<-new.parser()
#' add_rule(peg, "R<-'ab'")
#' #next we reset the rule
#' peg["R"]<-"R<-'r'"
#'
#' @export
"[.genE"<-function(parser, rule.id){
  if(!("genE" %in% class(parser))){ stop("first argument not a peg parser")}  
  #to return a attached rule (i hope)
  if(!("character" %in% class(rule.id))){
    stop("second argument is not a string")
  }
  if( rule.id %in% rule_ids(parser)){ #return an attached rule
    ar<-function(...){
      task<-"apply"
      applyRule<-function(...){
        dots<-list(...)
        input<-dots[[1]]
        #print(input)
        input.text<-input[1]
        exe= ifelse(is.null(input$exe), FALSE, input$exe==TRUE )
        debugTree=ifelse(is.null(input$debugTree), FALSE, input$debugTree==TRUE)  
        #print(debugTree)
        return(apply_rule(parser, rule.id, input.text, exe=exe, debugTree=debugTree ))
      }
      printRule<-function(){
        return(inspect_rule(parser,rule.id))
      }
   switch(task,
             apply = applyRule(list(...)),
             prt   = printRule() #,
      )
    }
    class(ar)<-"AttachedRule"
    ar
  } else { 
    #return null for now
    return(NULL)
  }
}

#' Prints an attached rule
#' 
#' @examples
#' peg<-new.parser()
#' add_rule(peg, "R<-'ab'")
#' print(peg["R"])
#' @export 
print.AttachedRule<-function(attached.rule){
  prt<-function(){}
#   bb<-body(attached.rule)
#   bb[2]<-call(task<-'prt')
#   body(attached.rule)<-bb
#   bb[2]<-call(task<-'prt')
  body(attached.rule)[2]<-call(task<-'prt')
  attached.rule()
  
}