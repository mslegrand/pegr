


# hint and tips
# for debugging:
# options(warn=2)
# options(error=recover)
# browser()
#____________________________________________________
# _____________________________________

create<-function(pegE){
  
#BEGIN GENERATOR PROPER
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
        id<-pegE$.new.ID()
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




#BEGIN CREATE
s.ID<-function(fn){ # Used only for P2 of new.generator::create
  h<-function(input,  exe, pos){
    fn(input,  exe, pos)
  }
  h
}  
#END OF PREP DEFS
#BEGIN OF CREATE PROPER
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
      cat("ERROR: EXPECTING A LIST, GOT INSTEAD ")
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
#END OF GENERATOR PROPER

