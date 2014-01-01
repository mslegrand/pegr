#MAIN PEG GENERATOR

#' Generate a pegR parser
#' 
#' 
#' This function generates a PEG parser 
#' 
#' @param grammer text of the PEG format used to generate the resulting parser
#' @param textGrammer 
#' @return an enviroment containing the Rule Objects
#' 
#' @keywords PEG parser grammer

generate<-function(grammer=text, testGrammer=FALSE, pegE=NULL  ){
  if(is.null(pegE)){
    pegE<-new.env() 
  }  
  source("sComponents.r", local=TRUE)
  source("literal.r", local=TRUE)
  source("generator.r",local=TRUE)
  GRAMMAR(grammer, !testGrammer)->res
  pegE$.status=res
  pegE
}
