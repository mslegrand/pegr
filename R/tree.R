#' Print Tree
#' 
#' Prints a tree representing the result of applying a rule to a text input, when that result was 
#' obtained with the record parameter set to TRUE
#' 
#' @param res, a result obtained from parsing with record=TRUE
#' @examples
#'   peg<-new.parser()
#'   add_rule(peg, "A<-'a'")
#'   add_rule(peg, "B<-'b'") 
#'   add_rule(peg, "D<-'d'")
#'   add_rule(peg, "C<-'c'")
#'   add_rule(peg,"ROOT<-A B C D")
#'   apply_rule(peg,"ROOT","abcd", record=TRUE)->res
#'   tree(res)
#' @export
tree<-function(res){
  if( is.null(res$debugNode) ){
    stop("Record option was not set, rerun parse with option record=TRUE", call. = FALSE)
  }
  #requires 2 passes, 1 for the links/levels, second actualy prints
  node.print<-function(n, indent="", lastChild=TRUE){
    #s<-paste(rep("",indent),collapse=" ")
    si<-paste0(indent,"__")
    arg<-n$data$consumes$t2 #paste(n$data$consumes, collapse="")
    s<-paste0(si,"__", n$data$name, "(",arg , ") = list(", n$data$value ," )","\n") 
    cat(s)
    if(lastChild==TRUE){
      substr(indent,nchar(indent),nchar(indent))<-" "
    }
    #indent2<-paste(indent,"    ")
    indent1<-paste(indent,"   |")
    indx=1
    if(length(n$children)>0){
      for(child in n$children){
        if(indx<length(n$children)){
          node.print(child, indent1, FALSE)
          indx<-indx+1
        } else {
          node.print(child, indent1, TRUE)
          indx<-1
        }     
      }
    }
  }
  node.print(res$debugNode[[1]]) #!!!!
}
