#todo: rewrite definition.node for post mortum actions

# hint and tips
# for debugging:
# options(warn=2)
# options(error=recover)
# browser()
#____________________________________________________
# _____________________________________



#' Creates an instance of a new PEG parser.
#' 
#' 
#' @export
#' @return Returns a new instance of a PEG parser  
#' @keywords PEG parser grammer
#' @examples
#' parser<-new.parser() 
#' add_rule(parser, "Any<-.")  
#' rule_ids(parser)  # returns "Any"
new.parser<-function(debugTree=FALSE){
  #internally we have two parsers, a genE a peg Generator which takes text and processes it
  #to create rules to construct the user defined parser, pegE. However, since the process
  #is to be dynamiclly interpetive (i.e. user can put in one rule at a time), the generator, genE
  #must be able to modify the pegE and hence needs to contain the pegE.
  pegE<-new.env()
  class(pegE)<-c("pegE",class(pegE))
  pegE$.DEBUG.NODE=debugTree
  pegE$.ACTION<-list() #executable for the rule
  pegE$.SOURCE.RULES<-list() #text containing the rule source, i.e A<-'c'
  pegE$.RULE_DESCRIPT<-list() #text containing the rule description
  pegE$.ACTION_NAMES<-list() #list containing the names of actions which are functions
  pegE$.AUTO_ACTION<-FALSE
  #source("node.r", local=TRUE)
  
  DEVEL.DEBUG<-FALSE
  
  #' Combines IDENTIFIER , LEFTARROW , EXPRESSION , opt.01(EXEC) to make a rule
  #' definition.Node is essentially the same as sequence except for when res$ok==FALSE
  #' Used in generator rule DEFINITION only
  #' Results are processed by mk.Rule<-function(defName, def, action)
  definition.Node<-function(...){ #!!!maybe we should rename this to definitionNode
    lf<-list(...) # consists of IDENTIFIER , LEFTARROW , EXPRESSION , opt.01(EXEC)
    h<-function(input, exe=TRUE,  p=1){  
      if(DEVEL.DEBUG){
        cat("sequenceN: full input=\n",input,"\n p=",p,"\n") ###good for debugging      
        cat("sequenceN: considering input=\n",substr(input,p,length(input)),"\n p=",p,"\n") ###good for debugging      
      } 
      mn<-0
      val=list()
      if(pegE$.DEBUG.NODE==TRUE){ 
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
        if(pegE$.DEBUG.NODE==TRUE){ 
          d.node<-c(d.node, res$debugNode) #!!!
        }     
      }
      if(DEVEL.DEBUG){
        cat("sequenceN: captured input=\n",substr(input,p,p+mn),"\n p=",p,"\n") ###good for debugging      
      } 
      res<-list(ok=TRUE, pos=mn, val=val)
      if(pegE$.DEBUG.NODE==TRUE){
#         defDiscription<-substr(input,p, p+mn) #!!!!we add the rule description here so that the debugger can use it later: this includes rule and action text   
#         #add def to enviroment
#         rule.id<-lf[[1]]
#         defList<-envir$.DEFS
#         defList$
#           assign(".DEFS", TRUE, envir=pegE)     
        res$debugNode=d.node
      }
      return(res)
      # when pegE$DEBUG.NODE==T we add to the results res an
      # additional field called debugNode (to get res$debugNode)
      # this field contains
      #     if(debug.Node()){
      #       return(list(ok=TRUE, pos=mn, val=val, debugNode=d.node)) #add here the consumed piece to recover the rule definition
      #     } else {
      #       return(list(ok=TRUE, pos=mn, val=val))      
      #     }    
    }
    class(h)<-c("pe",class(h))
    h
    #   fn<-memoize(h)
    #   fn
  }
  #___________________________________
  
  #Used exclusively by mk.Rule (below) to report bad rules
  debugging2.peg<-function(){
    return(exists("PEG.DEBUG.L2")) 
  }
  
  #' Generates new unique ID values for debug nodes #for new.ID only!!!!
  new.ID.generator<-function(){
    IDCount<-1
    getID<-function(){
      IDCount<<-IDCount+1
      paste("ID",IDCount,sep="-")
    }
    getID  
  } 
  new.ID<-new.ID.generator() #for mk.rule only!!!!
  
  #def should be a the user definition to add the action to.
  #defName is the user name of the def
  #defAction is the user action to be applied to the results of the definition
  #in terms of create
  #devName<-def {action}
  mk.Rule<-function(defName, def, action){
    #check on that mfn is  alist with message and function
    #   if(!("function" %in% class(mfn))){
    #     cat"Bad function"
    #     exit()
    #   }   
    #wrap
    if(is.null(action)){
      if(pegE$.AUTO_ACTION){
        pegE$.ACTION[[defName]]<-NULL
        pegE$.ACTION_NAMES[[defName]]<-NULL        
      }
    } else {
      pegE$.ACTION[[defName]]<-eval(parse(text=action)) #this parses the user action text for later invokation      
      pegE$.ACTION_NAMES[[defName]]=action
    }
    
    h<-function(input, exe=TRUE,  p=1){
      mfn.mssg<-defName
      #       if(is.null(action)){
      #         mfn.fn<-NULL  
      #       } else {
      #         mfn.fn<-eval(parse(text=action)) #this parses the user action text for later invokation      
      #       }
      # parse input
      res<-def(input, exe,  p)
      if(length(res$val)>0 & "character" %in% class(res$val)){
        mfn.mssg<-res$val[[1]]
        print(mfn.mssg) #!!!Why am I printing this????
      }
      
      if(res$ok==FALSE){   
        if(debugging2.peg()){ print(paste(mfn.mssg,"def Rejected: Exiting def: ",sep="=> ")) }
        return(res)
      } 
      else { #res$ok=TRUE
        if(debugging2.peg()){
          pp<-p+res$pos
          cat("      pp=",pp,"  :\n")
          #       print(paste(substr(input,1,pp-1)), substr(input,pp,nchar(input)), sep="|")      
          print(paste(mfn.mssg, "def value:", sep="=>"))
          tmp<-unlist(res$val)
          cat("      ")
          print(paste(tmp,collapse=", "))
        }    
        #execute rule action
        if(exe==TRUE  ){ #this is where we execute the user action
          # make this refer to a memmber of action array in pegE
          if(!is.null(pegE$.ACTION[[defName]])){
            res$val<-pegE$.ACTION[[defName]](res$val)          
          }
          #
          if( debugging2.peg() ){
            print(paste(mfn.mssg, "action value:", sep="=>"))
            tmp<-unlist(res$val)
            cat("      ")
            print(paste(tmp,collapse=", "))
          }
        }
        #add the debug node here
        if(pegE$.DEBUG.NODE==T){           
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
        if( debugging2.peg() ){
          print(paste(mfn.mssg, "def Succeeded: Exiting def:", sep="=>"))
        }   
      } #end of else: res$ok=TRUE
      res
    }
    class(h)<-c("rule",class(def))
    h
  }
  
  s.ID<-function(fn){ #used only for P2 of new.generator:create
    h<-function(input,  exe, pos){
      fn(input,  exe, pos)
    }
    h
  } 
  
  
  create<-function(pegE){
    #we source here so that pegE we use the pegE argument from generator (and not a global pegE)
    #source("sComponents.r", local=TRUE)
    DEVEL.DEBUG<-FALSE
    include.sComponents(pegE)
    include.sConnectives(pegE)
    include.gConnectives(pegE)
    #source("literal.r", local=TRUE)
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
        print(v)
        stop("bad rule syntax")
      }
      fn<-do.call(s.sequence, v); list(seq=fn)}) 
    EXPRESSION <- SEQUENCE + opt.0x(SLASH + SEQUENCE) < list("EXPRESSION", function(v){fn<-do.call(s.first,v); list(exp=fn)})
    DEFINITION <- definition.Node(IDENTIFIER , LEFTARROW , EXPRESSION , opt.01(EXEC)) < list(
      "DEFINITION", 
      function(v){ 
        name<-v[[1]]; fn<-v[[3]]; 
        #if EXEC exists, take that function (call it g) and apply 
        if("EXEC" %in% names(v)){ # g to the output of fn and wrap as a node (with nodeName embedded)
          userAction<-v$EXEC  
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
  genE<-create(pegE)
  #of course we can turn this around and set pegE$.genE<-genE
  #the future calls to add a rule would be pegE$genE$DEFINITION instead of genE$DEFINITION
  # however, what is probably better is to have a list with $genE, $pegE as members.
  # then, since we don't need to keep genE around unless we are adding a rule, we could
  # have just a structure, list(pegE, add_rule(pegE, rule) ), where 
  # add_rule<-function(pegE, rule){ genE<- create(pegE); genE$Definiton(rule)->result} 
  # in this case we would return the list(pegE, add_rule(pegE, rule) ), with a pegR class associated 
  # with it.
#   class(genE)<-c("pegR",class(genE))
#   genE
  pegR<-list(pegE=pegE, 
             getGenE=function(){genE}, #this is just for unit tests
             DEFINITION=function(ruleDef){ 
              #genE<-create(pegE); 
              genE$DEFINITION(ruleDef)
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
                 stop(paste("invalid syntax:",ruleDef))
               }
               res$rule.id=name.id
               res
             }, 
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
             SET_ACTION_TXT=function(rule.id, actionName){
               pegE$.ACTION_NAMES[[rule.id]]<-actionName 
             },
             GET_ACTION_TXT=function(rule.id){
               pegE$.ACTION_NAMES[[rule.id]]
             },            
             GET_IDS=function(){
               ls(envir=pegE)->tmp
               if( any(grepl("atom.",tmp) ) ){
                 tmp<-tmp[-grep("atom.",tmp)]    
               }
               tmp              
             },
             GET_RULE_STRUCTURE=function(rule.id){
               rs<-list(name=rule.id, 
                    def=GET_RULE_TXT(rule.id), 
                    com=GET_DESCRIPTION(rule.id),
                    act=GET_ACTION_TXT(rule.id)
               )
               class(rs)<-"ruleStructure"
             }
             )
  class(pegR)<-"pegR"
  pegR
}

#wrappers around the pegR to be more R like
pexSetRule<-function(pegR, rule){
  pegR$SET_RULE(rule)
}
pexGetRuleTxt<-function(pegR, rule.id){
  pegR$GET_RULE_TXT(rule.id)
}
pexSetDescription<-function(pegR, rule.id, description){
  pegR$SET_DESCRIPTION(rule.id, description)
}
pexGetDescription<-function(pegR, rule.id){
  pegR$GET_DESCRIPTION(rule.id)
}
pexSetAction<-function(pegR, rule.id, action){
  pegR$pegE$.ACTION[[rule.id]]<-action 
}
pexGetAction<-function(pegR, rule.id){
  pegR$pegE$.ACTION[[rule.id]]  
}
pexSetActionTxt<-function(pegR, rule.id, actionName){
  pegR$pegE$.ACTION_NAMES[[rule.id]]<-actionName 
}
pexGetActionTXT<-function(pegR, rule.id){
  pegR$pegE$.ACTION_NAMES[[rule.id]]
}
pexGetIDs<-function(pegR){
  pegR$GET_IDS()
}


# p.setDes(pegR, des)
# p.setAct(pegR, act)
# p.getRuleDef(pegR, id)
# p.getDes(pegR, id)
# p.getAct(pegR, id)
# p.getIDs(pegR)
# p.get