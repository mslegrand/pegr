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


#' Summarizes of the parser rule structure
#' 
#' Summary provides the following :
#' \enumerate{
#' \item \emph{Undefined Rules} This lists calls to rules that have not yet been defined.
#' \item \emph{Recursives} This list rules which may directly (or indirectly) call themselves
#' \item \emph{Roots} This lists rules which are not called by any rule and may only play the role
#' of a root.
#' \item \emph{Terminals} This lists rules which call do not call any rule and may only play the role
#' of a leaf (terminal)
#' }
#' 
#' @export
summary.pegR<-function(pegR){
  summary<-get.rule.call.summary(pegR)
  if(nrow(summary$missing)>0){
    undefined<-summary$missing$cid
    cat("Undefined Rules:", paste(undefined, collapse=", "), "\n")
  } else {
    cat("Undefined Rules: None\n")
  }
  if(length(summary$recursives)>0){
    cat("Recursives:", paste(summary$recursives, collapse=", "), "\n")
  } else {
    cat("Recursives: None\n")
  }
  if(length(summary$missing)>0){
    cat("Roots:", paste(summary$roots, collapse=", "), "\n")
  } else {
    cat("Rootss: None\n")
  }
  if(length(summary$terminals)>0){
    cat("Terminals:", paste(summary$terminals, collapse=", "), "\n")
  } else {
    cat("Terminals: None\n")
  }
}
