#BEGIN ID GENERATOR
# Generates new unique ID values for debugNode (see .RECORD.NODE)  #for new.ID only!!!!
new.ID.generator<-function(){
  IDCount<-1
  getID<-function(){
    IDCount<<-IDCount+1
    paste("ID",IDCount,sep="-")
  }
  getID  
} 
#END ID GENERATOR


#' Creates an instance of a new PEG parser.
#' 
#' @param peg.data.frame, a data frame with rules to populate the parser. (default is NULL) 
#'  A  peg.data.frame  consists of the following
#'  fields:
#'  \itemize{
#'  \item{rule.id} \emph{(mandatory)}, {The column containing the rule's id. \emph{(NA values not allowed)}} 
#'  \item{rule.source} \emph{(mandatory)}, {The  column containing the rule's definition \emph{(NA values not allowed)}} 
#'  \item{rule.description}, \emph{(optional)} {The column containing any rules description (may have NA values) }
#'  \item{action.type}, \emph{(optional)} {Indicates type of action ("Inline" or "External"). Set to  NA if no action is associated with this rule
#'  }
#'  \item{action.specification}, \emph{(optional)} {The column containing the rule specifiation. May be either a valid inline action or 
#'  the name of an existing rule (Set to NA if e no action is associated with this rule.)}
#' }  
#' @param record.mode, when set, will keep a record to display with \code{\link{tree}} or \code{\link{plot}}. (default is FALSE)
#' @return Returns a new instance of a PEG parser
#' @keywords PEG parser grammer
#' @examples
#' #Create an empty parser
#' parser<-new.parser() 
#' add_rule(parser, "Any<-.")  
#' rule_ids(parser)  # returns "Any"
#' 
#' #Create a parser from a data.frame
#' fn<-function(x){list()}
#' df<-data.frame(
#' rule.id=c('A','B'), 
#' rule.source=c("A<-'a'", "B<-'b'"), 
#' rule.description=c("aaa",NA),  
#' action.type=c("Inline","External"), 
#' action.specification=c("list()", "fn"), 
#' stringsAsFactors=FALSE)
#' peg<-new.parser(df)
#' @seealso \code{\link{as.data.frame}}
#' @export
new.parser<-function(peg.data.frame=NULL, record.mode=FALSE){
  #internally we have two parsers, a genE a peg Generator which takes text and processes it
  #to create rules to construct the user defined parser, pegE. However, since the process
  #is to be dynamiclly interpetive (i.e. user can put in one rule at a time), the generator, genE
  #must be able to modify the pegE and hence needs to contain the pegE.
  
  # NEW ENV FOR TO CONTAIN THE USER DEFINED PARSER
  pegE<-new.env()
  class(pegE)<-c("pegE",class(pegE))
  
  # RECORD FLAGS FOR TREE AND PLOTTING
  pegE$.new.ID<-new.ID.generator() #Used only by  mk.rule!!!! (may want to restart when setting RECORD.NODE on)
  pegE$.RECORD.NODE.DEFAULT<-record.mode
  pegE$.RECORD.NODE<-record.mode
  
  # CONTAINER FOR ACTUAL EXECUTABLE ACTIONS
  pegE$.ACTION<-list() #executable for the rule
  pegE$.AUTO_ACTION<-FALSE # MAY DEPRECATE AUTO ACTION: refers to action added inline from legacy text input
  pegE$.ACTION_DEFAULT<-FALSE #DEFAULT FOR RUNNING ACTIONS (BY DEFAULT THIS IS OFF)
  
  #TODO!!!??? PUT SOURCE.RULES, RULE DESCRIPT AND ACTION INFO INTO A SINGLE .RULES DATA FRAME???
  #           GOOD OR BAD IDEA???
  pegE$.SOURCE.RULES<-list() #text containing the rule source, i.e A<-'c'
  pegE$.RULE_DESCRIPT<-list() #text containing the rule description
  pegE$.ACTION_INFO<-list() #list containing the names of actions which are functions
  
  #STACK 
  pegE$.STOP_LEVEL<-Inf #use Inf to indicate that there is no stop level (allow infinite deep recursion)
  pegE$.STACK<-data.frame() #RULE STACK TO BE RECORDED WHEN STACK STOP LIMIT IS SET
  
  #BEGIN DEBUGGER  
  pegE$.DEBUG_ON<-FALSE # Flag rules use to check, to see if any debugging is on
  pegE$.DEBUG<-list( #Structure to hold debugger related data
    SIMULATION=c(), # used for knitr docs (enter here a sequence of debugger commands to simulate user input)
    NEXT=TRUE,           #TRUE is next, FALSE is continue
    BRKPTS=data.frame(id=NA, at=NA)[numeric(0), ],
    command.summary=function(){cat("Rdb> Commands: h, n, c, clr, +brk@, -brk@, Q, r, l\n")},
    command.detail=function(){
      cat(
        "Command Summary",
        "h, help: shows this help",
        "n: step to the next rule",
        "c: continue to the next breakpoint",
        "clr: clear all breakpoints",
        "+brk@: add break point at both enter and exit points of a rule",
        "     : example +brk@ RULE.ID",
        "+brk@>: add break point at the enter point of a rule",
        "     : example +brk@> RULE.ID",
        "+brk@<: add break point at the exit point of a rule",
        "     : example +brk@< RULE.ID",
        "-brk@: delete break points of a rule",
        "-brk@>: delete break point at the enter point of a rule",
        "     : example -brk@> RULE.ID",
        "-brk@<: delete break point at the exit point of a rule",
        "     : example -brk@< RULE.ID" , 
        "value: display the return value (upon exiting with statusa list)",
        "q: quit the debugger",
        "r: restart debugger",
        "l: list all rule breakpoints",
        sep="\n"
      )
    }
  ) #end of pegE$.DEBUG list
  
  #main debug looP MAY WANT TO MOVE THIS INSIDE pegE$.DEBUG
  pegE$.debug.loop<-function(res=NULL){  
    repeat{ #forever
      #if simulation()
      if(length(pegE$.DEBUG$SIMULATION)>0){
        #pop the first entry
        line<-pegE$.DEBUG$SIMULATION[1]
        pegE$.DEBUG$SIMULATION<-pegE$.DEBUG$SIMULATION[-1] 
        #echo to terminal simated command
        cat("Rdb>", line, "\n")
      } else {
        line<-str_trim(readline("Rdb>"))        
      }      
      if(line==""){
        #repeat the  last command (n or c)
        return()
      }
      tolower(line)->cmd
      #convert cmd to lower
      if(grepl("^[+-]brk@", cmd)){ #break
        #we have a break point
        str_match(line, "^([+-])brk@\\s*([<>]?)\\s*(.*)")->mat
        type.ad<-mat[2]
        type.ee<-mat[3]     
        rule.id<-mat[4]
        #check for valid rule id (if not err msg and continue)
        if( type.ee %in% c('<', '>')){ #either exit or enter
          if(type.ad=='+'){ #we add
            pegE$.DEBUG$BRKPTS<-rbind(pegE$.DEBUG$BRKPTS, data.frame(id=rule.id, at=type.ee))
          } else { #we delete
            pegE$.DEBUG$BRKPTS<-with(pegE$.DEBUG$BRKPTS,
                                     subset(pegE$.DEBUG$BRKPTS, !( id==rule.id & at==type.ee) ) ) 
          }
        } else { # both exit and enter
          if(type.ad=='+'){ #we add
            pegE$.DEBUG$BRKPTS<-rbind(pegE$.DEBUG$BRKPTS, data.frame(id=rule.id, at=c('<','>')))            
          } else { #we delete
            pegE$.DEBUG$BRKPTS<-with(pegE$.DEBUG$BRKPTS,
                                     subset(pegE$.DEBUG$BRKPTS, !( id==rule.id) ) )
          }        
        } #end of both exit and enter
      } #end of break
      else{ #not a breakpoint insertion/deletion so process the other commands
        switch(EXPR=cmd,
               c={
                 pegE$.DEBUG$NEXT<-FALSE
                 break #exit from loop
               },
               n={
                 pegE$.DEBUG$NEXT<-TRUE
                 break #exit from loop
               },
               q=, #"bail"
               quit={ 
                 invokeRestart("quitDebug")
               },
               r={ #run from the beginning
                 invokeRestart("restartDebug")
               },
               h=, #display help
               help={ 
                 pegE$.DEBUG$command.detail() 
               },
               l=, #prings all break points
               list={
                 cat("PEG Breakpoint Listing:\n")
                 if(nrow(pegE$.DEBUG$BRKPTS)>0){
                   ll<-pegE$.DEBUG$BRKPTS[with(pegE$.DEBUG$BRKPTS, order(id,at)),]
                   apply(ll, 1, function(x){cat("brk@ ",x[2]," ",x[1], "\n" )})                 
                 } else {
                   cat("No PEG break points set!\n")
                 }
               },
               clr={ #clear all break points
                 pegE$DEBUG$BRKPTS=data.frame(id=NA, at=NA)[numeric(0), ]
               },
               v=, #prints the value list
               value={
                 if(is.null(res)){
                   cat("Return Value Not Availabe\n")
                 } else  {
                   cat("Returned value:\n")
                   print(res$val)                 
                 }
               }
               
        ) #end of switch 
      } #end of other commands
    } #end of repeat
  } #end of pegE$.debug.loop
  
  #END DEBUGGER
  
  
  
  #genE<-create(pegE)
  
  include.readDF()
  
  if(!is.null(peg.data.frame)){
    # 1. assert df is of class dataframe
    # 2. add df to 
    genE<-create(pegE)
    ok<-add_data.frame(genE,peg.data.frame)
    # 3. if failed, return null
    if(!ok){
      return(NULL)
    } 
  }
  #
  #??? Shall we do this differently
  get_IDS<-function(){
    ls(envir=pegE)->tmp
    if( any(grepl("atom.",tmp) ) ){
      tmp<-tmp[-grep("atom.",tmp)]    
    }
    tmp              
  } # end get_IDS
  
  #BEGIN pegR OBJECT
  pegR<-list(pegE=pegE, 
             #getGenE=function(){genE}, #this is just for unit tests
             DEFINITION=function(ruleDef){ 
               genE<-create(pegE); 
               genE$DEFINITION(ruleDef)
             },
             GET_DATA_FRAME=function(...){
               df<-data.frame(rule.id=NA, rule.definition=NA, 
                              rule.description=NA, action.type=NA, 
                              action.specification=NA, stringsAsFactors=FALSE, ...)[numeric(0),]
               #loop over rules and extract
               ids<-get_IDS()
               for(id in ids){
                 rule.id<-id
                 rule.definition<-pegE$.SOURCE.RULES[[id]]
                 rule.description<-pegE$.RULE_DESCRIPT[[id]]
                 rule.description<-ifelse(is.null(pegE$.RULE_DESCRIPT[[id]] ), NA, pegE$.RULE_DESCRIPT[[id]])
                 if(is.null(pegE$.ACTION_INFO[[id]])){
                   #action<-NA 
                   action.specification<-NA
                 } else {
                   action.info<-pegE$.ACTION_INFO[[id]]
                   #action.type<-action.info[1]
                   action.specification<-action.info #[2]
                 }
                 df1<-data.frame(rule.id=rule.id, rule.definition=rule.definition, 
                                 rule.description=rule.description,  
                                 action.specification=action.specification, stringsAsFactors=FALSE, ...)
                 df<-rbind(df,df1)
               }
               df
             },
             SET_RULE=function(ruleDef){
               #res<-parser$DEFINITION(pegE, ruleDef) 
               genE<-create(pegE)
               res<-genE$DEFINITION(ruleDef)
               if(res$ok==TRUE){
                 name.id<-strsplit(ruleDef,"<-")[[1]][1]
                 name.id <- gsub("^\\s+|\\s+$", "", name.id)
                 pegE$.SOURCE.RULES[[name.id]]<-ruleDef   
               } else {
                 stop(paste("invalid syntax:",ruleDef), call. = FALSE)
               }
               res$rule.id=name.id
               res
             }, #end SET_RULE
             GET_RULE_TXT=function(rule.id){
               pegE$.SOURCE.RULES[[rule.id]]
             },
             SET_DESCRIPTION=function(rule.id, description){
               pegE$.RULE_DESCRIPT[[rule.id]]<-description
             }, 
             GET_DESCRIPTION=function(rule.id){
               pegE$.RULE_DESCRIPT[[rule.id]]
             },
             SET_ACTION=function(rule.id, action){
               pegE$.ACTION[[rule.id]]<-action 
             },
             GET_ACTION=function(rule.id){ #not used?
               pegE$.ACTION[[rule.id]]
             },
             SET_ACTION_INFO=function(rule.id, actionInfo){
               pegE$.ACTION_INFO[[rule.id]]<-actionInfo 
             },
             GET_ACTION_INFO=function(rule.id){
               pegE$.ACTION_INFO[[rule.id]]
             },            
             GET_IDS=function(){
               return(get_IDS())       
             }, # end GET_IDS
             APPLY_RULE=function(rule.id,input.text, exe.Action=NULL, record=NULL){
               # prep
               #
               # prep: action
               exe.Action<-ifelse(is.null(exe.Action), pegE$.ACTION_DEFAULT, exe.Action)
               #clear stack
               if(is.finite(pegE$.STOP_LEVEL)){
                 pegE$STACK<-data.frame()
               } 
               # prep: recording mode
               record<-ifelse(is.null(record), pegE$.RECORD.NODE.DEFAULT, record)
               pegE$.RECORD.NODE<-record
               #if debugging set show help menu
               # exec
               pegE[[rule.id]](input.text, exe.Action)->res
               # clean up
               # if debugging show exit status
               res$Call<-list(rule.id=rule.id, arg=input.text)
               res$options<-list(exe=exe.Action, record=pegE$.RECORD.NODE )
               res
             }, #end APPLY_RULR
             SET_STOP_LEVEL=function(limit){
               pegE$.STOP_LEVEL<-limit
               pegE$STACK<-data.frame()
             },
             UNSET_STOP_LEVEL=function(){
               pegE$.STOPLEVEL<-Inf
               pegE$STACK<-data.frame()
             },
             GET_STACK=function(){
               pegE$.STACK
             },
             SET_RECORD_DEFAULT=function(){
               pegE$.RECORD.NODE.DEFAULT=on
             },
             SET_DEBUG_ON=function(mode){
               pegE$.DEBUG_ON=mode
               if(pegE$.DEBUG_ON) {
                 pegE$.DEBUG$BRKPTS<-data.frame(id=NA, at=NA)[numeric(0), ]
                 pegE$.DEBUG$NEXT<-TRUE
               }
             },
             DELETE_RULE=function(rule.id){
               pegE$.SOURCE.RULES[[rule.id]]<-NULL
               pegE$.ACTION[[rule.id]]<-NULL
               pegE$.RULE_DESCRIPT[[rule.id]]<-NULL
               pegE$.ACTION_NAMES[[rule.id]]<-NULL
               rm(list=rule.id, envir=pegE)                  
             },
             GET_DEBUG_ON=function(){
               pegE$.DEBUG_ON
             } 
  )
  #class(pegR)<-c("pegR",class(pegR))
  class(pegR)<-c("pegR")
  
  # END OF pegR OBJECT
  
  pegR # Return pegR object
}
# END OF new.parser
