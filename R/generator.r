


# hint and tips
# for debugging:
# options(warn=2)
# options(error=recover)
# browser()
#____________________________________________________
# _____________________________________


#' Creates an instance of a new PEG parser.
#' 
#' @param peg.data.frame, a data frame with rules to populate the parser. (default is NULL) A peg.data.frame  consists of the following
#'  fields:
#' * *rule.id*,  column containing the rule's id (mandatory)
#' * *rule.source*, a  column containing the rule's definition (mandatory)
#' * *rule.description* a column to contain a rules description (optional, and may contain NA)
#' * *action.type* a column indicating the type of action ("Inline" or "External") (optional if there are no actions, may contain NA)
#' * *action.specification*, a column containg either a valid inline action or the name of an existing rule (optional if there are no actions)
#' @param record.mode, when set, will keep a record to display with \code{\link{tree}} or \code{\link{plot}. (default is FALSE)
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

#BEGIN DEFINITION NODE
  #' Combines IDENTIFIER , LEFTARROW , EXPRESSION , opt.01(EXEC) to make a rule
  #' definition.Node is essentially the same as sequence except for when res$ok==FALSE
  #' Used in generator rule DEFINITION only
  #' Results are processed by mk.Rule<-function(defName, def, action)
  definition.Node<-function(...){ #!!!maybe we should rename this to definitionNode
    lf<-list(...) # consists of IDENTIFIER , LEFTARROW , EXPRESSION , opt.01(EXEC)
    h<-function(input, exe=TRUE,  p=1){  
      mn<-0
      val=list()
      if(pegE$.RECORD.NODE==TRUE){ 
        d.node<-list()
      }
      for(f in lf){
        if("peg.name" %in% class(f)){ if(exists(f,envir=pegE)){ f<-get(f,envir=pegE)}}
        res<-f(input,  exe, p+mn)
        if(res$ok==FALSE){
          return(list(ok=FALSE,pos=0, val=val )) #this line is the essential difference between sequenceNode and sequence
        }
        mn<-mn+res$pos
        val<-c(val,res$val)
        if(pegE$.RECORD.NODE==TRUE){ 
          d.node<-c(d.node, res$debugNode) #!!!
        }     
      }
      res<-list(ok=TRUE, pos=mn, val=val)
      if(pegE$.RECORD.NODE==TRUE){
        # when pegE$DEBUG.NODE==T we add to the results res an
        # additional field called debugNode (to get res$debugNode)
        res$debugNode=d.node
      }
      return(res)
    }
#     class(h)<-c("pe",class(h))
#     h
    fn<-memoise::memoize(h)
    class(fn)<-c("pe",class(fn))
    fn
  } 
#END OF DEFINITION NODE

#BEGIN ID GENERATOR
  #' Generates new unique ID values for debugNode (see .RECORD.NODE)  #for new.ID only!!!!
  new.ID.generator<-function(){
    IDCount<-1
    getID<-function(){
      IDCount<<-IDCount+1
      paste("ID",IDCount,sep="-")
    }
    getID  
  } 
  new.ID<-new.ID.generator() #Used only by  mk.rule!!!!
#END ID GENERATOR

#BEGIN MAKE RULE
  #def:   the user rule (rule.souce) to add the action to.
  #defName: the rule.id
  #action:  an inline action for that rule (may deprecate this )
  mk.Rule<-function(defName, def, action){ 
    #  action is an inline action (may deprecate)

    #ADJUST THE ACTION WHEN AUTO_ACTION IS ON AND WE HAVE AN INLINE ACTION
    if(is.null(action)){ #if no inline action
      if(pegE$.AUTO_ACTION){ # and inline action flag is on
        pegE$.ACTION[[defName]]<-NULL   #null the actions
        pegE$.ACTION_INFO[[defName]]<-NULL        
      }
      # no inline action and auto is off so we do nothing (ie. keep status)
    } else {
      #if there is inline action we overwrite the action no matter what the flag is (probably not what we wanted!!!!)
      pegE$.ACTION[[defName]]<-eval(parse(text=action)) #this parses the user action text for later invokation      
      pegE$.ACTION_INFO[[defName]]=action
    }
    
    #h IS A WRAPPER WHICH CALLS def, def comes from definition.node (the sequence on the rhs of <- before {})
    h<-function(input, exe=TRUE,  p=1){
      mfn.mssg<-defName #record the rule.id for latter (say for when record=T)
      #this is before the node fn is executed (the node fn is def)
      #THIS IS A GOOD PLACE FOR DEBUGGER TO RECORD ENTERING A RULE
      if(pegE$.DEBUG_ON==TRUE){
        if(pegE$.DEBUG$NEXT | ( any(with(pegE$.DEBUG$BRKPTS, (id==defName & at=='>')  ) )    )    ){
          cat("==>Entering Rule:", defName, "\n")
          cat("   Rule Definiton:", pegE$.SOURCE.RULES[[defName]], "\n")
          cat("   Input text: \'", substr(input,p, nchar(input)),"\'\n", sep="")      
          pegE$.debug.loop()          
        } 
      }
      #THIS MAY BE A GOOD PLACE TO RECORD ENTERING RULE
      if( is.finite( pegE$.STOP_LEVEL ) ) {
        if( nrow(pegE$.STACK)>=pegE$.STOP_LEVEL)  { #bail
          stop("Max Call Depth of Rule Stack Exceeded! To see calling sequence use get_rule_stack", call. = FALSE)
        } 
        #else add to stack
        pegE$.STACK<-rbind(pegE$.STACK, data.frame(node.id=defName, pos=p ))
      }
      
      # *******************BEGIN EXECUTION OF THE RULE NODE HERE***********
      res<-def(input, exe,  p)
      # *******************END EXECUTION OF THE RULE NODE HERE************
      
      #THIS WOULD BE WHERE WE RECORD EXITING RULE (OR SHALL WE DO IT AFTER USER EXE IS DONE?)
      if(is.finite(pegE$.STOP_LEVEL)){
        pegE$.STACK<-pegE$.STACK[-nrow, ]
      }
      # THIS POTENTIAL PLACE FOR DEBUGGER TO RECORD EXITING IS A RULE      
      # BUT THE ACTION HAS NOT BEEN APPLIES SO WE PLACE IN
      # ALTERNATIVES:  2 PLACES: 
      #  1. AFTER RES$OK==FALSE
      #  2. AFTER RES$OK==TRUE AND ACTION HAS BEEN EXECUTED
      
#      #A NODE SHOULD NEVER RETURN ANYTHING BUT A LIST!!!
#       if(res$ok==TRUE & !list %in$ class(res$val)){
#         stop("Value did not return a list! (Bad Action?)")
#       }
      
      # if node fails and debugging print something
      if(res$ok==FALSE){   
        # DEBUGGER: EXITING NODE (ALTERNATIVE PART 1)
        if(pegE$.DEBUG_ON==TRUE){ 
          if(pegE$.DEBUG$NEXT | ( any(with(pegE$.DEBUG$BRKPTS, (id==defName & at=='<')  ) )    )    ){
            cat("<==Exiting Rule:", defName, "\n")
            cat("   Rule Definiton:", pegE$.SOURCE.RULES[[defName]], "\n")
            cat("   Status: ",res$ok," ; Rule ", defName," rejected the input: '", substr(input,p, nchar(input)),"'\n", sep="")
            cat("   Consumed: '", substr(input, p, p-1 +res$pos), "'\n", sep="") #pos is number of characters consumend
            pegE$.debug.loop()          
          } 
        }
        return(res)
      } 
      else { # ELSE THE NODE HAS SUCCEEDED: # res$ok=TRUE
        # EXECUTE ACTION IF THERE IS ONE
        if(exe==TRUE  ){ #this is where we execute the user action         
          if(!is.null(pegE$.ACTION[[defName]])){ # refers to a memmber of action array in pegE
            res$val<-pegE$.ACTION[[defName]](res$val)          
          }
        }
        # DEBUGGER: EXITING NODE (ALTERNATIVE PART 2)
        if(pegE$.DEBUG_ON==TRUE){
          if(pegE$.DEBUG$NEXT | ( any(with(pegE$.DEBUG$BRKPTS, (id==defName & at=='<')  ) )    )    ){
            cat("<==Exiting Rule:", defName, "\n")
            cat("   Rule Definiton:", pegE$.SOURCE.RULES[[defName]], "\n")
            cat("   Status: ",res$ok," ; Rule ", defName," accepted the input '", substr(input,p, nchar(input)),"'\n", sep="")
            #cat("   Status:",res$ok,"\n")
            cat("   Consumed: '", substr(input, p, res$pos), "'\n", sep="")
            pegE$.debug.loop(res)          
          } 
        }
        # ADD RECORD NODE HERE (FOR TREE AND PLOT )
        if(pegE$.RECORD.NODE==TRUE){  # record is TRUE, (for tree and plot)        
          #get res$debug list.
          children<-res$debugNode
          #add new node with this list
          id<-new.ID()
          msg.name<-mfn.mssg
          pp<-p+res$pos-1
          msg.consumes<-list(
            t1=substr(input,1, p-1),
            t2=substr(input,p, pp),
            t3=substr(input, pp+1, nchar(input))
          )
          #msg.consumes<-list(start=p,end=pp)
          msg.value<-paste(unlist(res$val),collapse=", ")        
          data<-list(name=msg.name,consumes= msg.consumes, value= msg.value)
          node<-new.node(id,msg.name,data=data, children=children)
          res$debugNode<-list(node=node) #!!!
        }       
      } #end of else: i.e. #THE NODE  SUCCEEDED: res$ok=TRUE
      res
    }
    class(h)<-c("rule",class(def))
    h
  } 
#END MAKE RULE


#??? Shall we do this differently
get_IDS<-function(){
  ls(envir=pegE)->tmp
  if( any(grepl("atom.",tmp) ) ){
    tmp<-tmp[-grep("atom.",tmp)]    
  }
  tmp              
} # end get_IDS


#BEGIN CREATE
  s.ID<-function(fn){ # Used only for P2 of new.generator::create
    h<-function(input,  exe, pos){
      fn(input,  exe, pos)
    }
    h
  }   
  create<-function(pegE){
    include.sComponents(pegE)
    include.sConnectives(pegE)
    include.gConnectives(pegE)
    include.literal(pegE)
    
    #PEG GENERATION RULES
    #--Grammer for the parser generator
    #Lexical syntax
    ENDOFFILE<-s.not(s.dot)
    ENDOFLINE<-literal("\n")
    SPACE<-s.first(literal(' '), literal('\t'), ENDOFLINE) < list("",function(v){list()}) #eat space
    COMMENT<-s.sequence(literal('#'), opt.0x( s.not(ENDOFLINE) + s.dot ), ENDOFLINE) < list("",function(v){list()}) #eat comment
    ENDOFEXE<- literal('}') #< list("",function(v){list()}) #eat#  / ENDOFLINE
    BEGOFEXEC<- literal('{') #< list("",function(v){list()}) #eat
    EXEC<-s.sequence( BEGOFEXEC, opt.0x(s.not(ENDOFEXE)+s.dot), opt.01(ENDOFEXE) ) <list("", function(v){ v<-paste(v,collapse=""); v<-substr(v,2,nchar(v)-1);   list(EXEC=v) })
    
    #ENDOFEXE<- literal('#') / ENDOFLINE
    #BEGOFEXEC<- literal('<<') < list("",function(v){list()}) #eat
    #EXEC<-s.sequence( BEGOFEXEC, opt.0x(s.not(ENDOFEXE)+s.dot), opt.01(ENDOFLINE) ) <list("", function(v){ v<-paste(v,collapse=""); list(EXEC=v) })
    
    SPACING<-opt.0x(SPACE /  COMMENT)
    DOT<-literal('.') + SPACING < list("dot",function(v){list(leaf=s.dot)})
    CLOSE<-literal(')') + SPACING < list("",function(v){list()}) #eat
    OPEN<-literal('(') + SPACING < list("",function(v){list()}) #eat
    PLUS<-literal('+') + SPACING
    STAR<-literal('*') + SPACING
    QUESTION<-literal('?') + SPACING
    NOT<-literal('!') + SPACING
    AND<-literal('&') + SPACING
    SLASH<-literal('/') + SPACING < list("",function(v){list()}) #eat
    LEFTARROW<-literal('<-') + SPACING
    QUOTE1<-literal("\'")
    QUOTE2<-literal("\"")
    BACKSLASH<-literal("\\")
    
    CHAR <- s.not(BACKSLASH) + s.dot
    RANGE <- (CHAR + literal('-') + CHAR) / CHAR
    CLASS <-  s.sequence(literal('[') ,  opt.0x( s.not( literal(']') )  + RANGE) , literal(']') , SPACING) < list("range", function(v){a<-mk.rng.a(v); list(leaf=a)})
    LITERAL <- (s.sequence(QUOTE1 , opt.0x(s.not(QUOTE1) + CHAR) ,  QUOTE1 , SPACING)  /
                  s.sequence(QUOTE2 , opt.0x(s.not(QUOTE2) + CHAR) ,  QUOTE2 , SPACING) ) < list("atom", function(v){a<-mk.l.a(v);  list(leaf=a)} )
    
    IDENTSTART<-s.range('a','z') / s.range('A','Z') 
    IDENTCONT<-IDENTSTART / s.range('0','9')
    IDENTIFIER<-IDENTSTART + opt.0x(IDENTCONT) + SPACING < list("", function(v){ v<-paste(v,collapse="");class(v)<-c("peg.name",class(v)); list(ID=v) } )
    
    #Hierarchical Syntax
    
    P1 <- IDENTIFIER +  s.not(LEFTARROW) #<list("P1",NULL) 
    P2 <-s.sequence( OPEN , s.ID(EXPRESSION) , CLOSE) #<list("P2,NULL")
    P3 <- s.first( LITERAL , CLASS ,DOT) #< list("P3",NULL)
    
    PRIMARY <- s.first( P1 , P2, P3) < list("PRIMARY",NULL)
    
    SUFFIX <- PRIMARY + opt.01( QUESTION / STAR / PLUS) < list("SUFFIX", function(v){ fn<-mk.suffix(v); list(suf=fn) } )
    PREFIX <- opt.01(AND / NOT) + SUFFIX < list("PREFIX", function(v){ fn<-mk.prefix(v); list(pre=fn) } )
    SEQUENCE <- opt.0x(PREFIX) < list("SEQ",function(v){ 
      if(!("list" %in% class(v))){
        cat("ERROR: EXPECTING A LIST, GOT INSTEAD")
        print(v)
        stop("BAD ACTION: RETURN MUST BE A LIST", call. = FALSE)
      }
      fn<-do.call(s.sequence, v); list(seq=fn)}) 
    EXPRESSION <- SEQUENCE + opt.0x(SLASH + SEQUENCE) < list("EXPRESSION", function(v){fn<-do.call(s.first,v); list(exp=fn)})
    DEFINITION <- definition.Node(IDENTIFIER , LEFTARROW , EXPRESSION , opt.01(EXEC)) < list(
      "DEFINITION", 
      function(v){ 
        name<-v[[1]]; fn<-v[[3]]; 
        #if EXEC exists, take that function (call it g) and apply 
        if("EXEC" %in% names(v)){ # g to the output of fn and wrap as a node (with nodeName embedded)
          userAction<-v$EXEC  # this is for inline action 
        }else{
          userAction<-NULL
        }
        fn<-mk.Rule(name,fn,userAction)
        assign(name, fn, envir = pegE); list(nodeName=name) 
      } 
    )
    GRAMMAR <- SPACING + opt.1x(DEFINITION) 
    environment() #return  of the generator, which will contain a peg object   
  }
# END CREATE

  # of course we can turn this around and set pegE$.genE<-genE
  # the future calls to add a rule would be pegE$genE$DEFINITION instead of genE$DEFINITION
  # however, what is probably better is to have a list with $genE, $pegE as members.
  # then, since we don't need to keep genE around unless we are adding a rule, we could
  # have just a structure, list(pegE, add_rule(pegE, rule) ), where 
  # add_rule<-function(pegE, rule){ genE<- create(pegE); genE$Definiton(rule)->result} 
  # in this case we would return the list(pegE, add_rule(pegE, rule) ), with a pegR class associated 
  # with it.

genE<-create(pegE)

include.readDF()

if(!is.null(peg.data.frame)){
  # 1. assert df is of class dataframe
  # 2. add df to genE
  ok<-add_data.frame(genE,peg.data.frame)
  # 3. if failed, return null
  if(!ok){
    return(NULL)
  } 
}
#

#BEGIN pegR OBJECT
  pegR<-list(pegE=pegE, 
             getGenE=function(){genE}, #this is just for unit tests
             DEFINITION=function(ruleDef){ 
              #genE<-create(pegE); 
              genE$DEFINITION(ruleDef)
             },
             GET_DATA_FRAME=function(...){
               df<-data.frame(rule.id=NA, rule.source=NA, 
                              rule.description=NA, action.type=NA, 
                              action.specification=NA, stringsAsFactors=FALSE, ...)[numeric(0),]
               #loop over rules and extract
               ids<-get_IDS()
               for(id in ids){
                 rule.id<-id
                 rule.source<-pegE$.SOURCE.RULES[[id]]
                 rule.description<-pegE$.RULE_DESCRIPT[[id]]
                 rule.description<-ifelse(is.null(pegE$.RULE_DESCRIPT[[id]] ), NA, pegE$.RULE_DESCRIPT[[id]])
                 if(is.null(pegE$.ACTION_INFO[[id]]) | is.null(pegE$.ACTION_INFO[[id]])){
                   action.type<-NA 
                   action.specification<-NA
                 } else {
                   action.info<-pegE$.ACTION_INFO[[id]]
                   action.type<-action.info[1]
                   action.specification<-action.info[2]
                 }
                 df1<-data.frame(rule.id=rule.id, rule.source=rule.source, 
                                 rule.description=rule.description, action.type=action.type, 
                                 action.specification=action.specification, stringsAsFactors=FALSE, ...)
                 df<-rbind(df,df1)
               }
               df
             },
             SET_RULE=function(ruleDef){
               #res<-parser$DEFINITION(pegE, ruleDef) 
               #genE<-create(pegE)
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

