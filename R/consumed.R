
#' Returns The portion ot the input text consumed during a parsing
#' 
#' @param res, a result obtained from parsing
#' @return The portion ot the input text consumed during the parse
#' 
#' #' @details Applying a rule to an input string will consume only the portion for which
#' there is a match. If no match, is consumed is "", then the status is FALSE, 
#' but even if there is a partial match, the status will be TRUE. In debugging
#' it may be useful to see how far the parser succeeded. If a complete is 
#' required then end the rule with \bold{!.}, meaning no more charactes.
#' 
#' @examples
#' peg<-new.parser()
#' add_rule(pe)
#' 
#' #A returns TRUEfor "aab" but consumes on the first 2 charactes
#' peg<-new.parser()
#' add_rule(peg, "A <- ('a' A) / 'a' ")
#' apply_rule(peg, "A", "aab")->res
#' status(res
#' consumed(res)
#' 
#' #All returns FALSE on "aab"
#' add_rule(peg, "All <- A !. ")
#' apply_rule(peg, "All", "aab")->res
#' status(res)
#' consumed(res)
#' #All return TRUE on "aaa"
#' apply_rule(peg, "All", "aaa")->res
#' status(res)
#' consumed(res)
#' 
#' #Another approach: change A to
#' #This now returns FALSE on "aab
#' add_rule(peg, "A<- ('a' A ) / ('a' !.)")
#' apply_rule(peg, "A", "aab")->res
#' status(res)
#' consumed(res)
#' #But returns true on: "aaa"
#' apply_rule(peg, "A", "aaa")->res
#' status(res)
#' consumed(res)
#' 
#' @export
consumed<-function(res){
  if(!("PEGResult" %in% class(res))){ stop("Argument not a peg parsing result")} 
  return(substring(res$Call$arg,1,res$pos))  
}
