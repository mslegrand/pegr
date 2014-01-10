#' An alternative to apply_rule
#' 
#' 
#' @examples
#' peg<-new.parser()
#' add_rule(peg, "R<-'ab'")
#' peg["R"]('ab') #equivalent to **apply_rule(peg, "R", 'ab')**
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
        input.text<-dots[[1]]
        exe= ifelse(is.null(dots$exe), FALSE, dots$exe==TRUE )
        debugTree=ifelse(is.null(dots$debugTree), FALSE, dots$exe==TRUE)        
        return(apply_rule(parser, rule.id, input.text, exe=exe, debugTree=debugTree ))
      }
      printRule<-function(){
        return(inspect_rule(parser,rule.id))
      }
      switch(task,
             apply = applyRule(list(...)),
             prt   = printRule(),
             set   = resetRule
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
