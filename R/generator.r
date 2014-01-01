#todo: rewrite definition.node for post mortum actions

# hint and tips
# for debugging:
# options(warn=2)
# options(error=recover)
# browser()
#____________________________________________________
# _____________________________________


#used in  unittest4 unitTest4 
as.ID=function(v){
  class(v)<-c("peg.name",class(v))
  v
}

new.generator<-function(debugger=FALSE){
  #internally we have two parsers, a genE a peg Generator which takes text and processes it
  #to create rules to construct the user defined parser, pegE. However, since the process
  #is to be dynamiclly interpetive (i.e. user can put in one rule at a time), the generator, genE
  #must be able to modify the pegE and hence needs to contain the pegE.
  pegE<-new.env()
  class(pegE)<-c("pegE",class(pegE))
  pegE$.DEBUG.NODE=debugger
  pegE$.ACTION<-list()
  pegE$.SOURCE.RULES<-list()
  pegE$.RULE_DESCRIPT<-list()
  
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
      if(pegE$.DEBUG.NODE==T){ 
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
        if(pegE$.DEBUG.NODE==T){ 
          d.node<-c(d.node, res$debugNode)
        }     
      }
      if(DEVEL.DEBUG){
        cat("sequenceN: captured input=\n",substr(input,p,p+mn),"\n p=",p,"\n") ###good for debugging      
      } 
      res<-list(ok=TRUE, pos=mn, val=val)
      if(pegE$.DEBUG.NODE==T){
        defDiscription<-substr(input,p, p+mn) #!!!!we add the rule description here so that the debugger can use it later: this includes rule and action text   
        #add def to enviroment
        rule.id<-lf[[1]]
        defList<-envir$.DEFS
        defList$
          assign(".DEFS", TRUE, envir=pegE)     
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
      pegE$.ACTION[[defName]]<-NULL  
    } else {
      pegE$.ACTION[[defName]]<-eval(parse(text=action)) #this parses the user action text for later invokation      
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
          res$debugNode<-node
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
    include.gComponents(pegE)
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
    SEQUENCE <- opt.0x(PREFIX) < list("SEQ",function(v){ fn<-do.call(s.sequence, v); list(seq=fn)}) 
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
  class(genE)<-c("genE",class(genE))
  genE
}

print.pegE<-function(pegE){
  #list the rules in this peg
  ls(envir=pegE)
}

AddRule<-function(genE, rule){
  if( !( "genE" %in% class(genE) ) ){ stop("first argument not a generator") }  
  res<-genE$DEFINITION(rule) 
  if(res$ok==TRUE){
    name<-strsplit(rule,"<-")[[1]][1]
    genE$pegE$.SOURCE.RULES[[name]]<-rule    
  } else {
    stop(paste("invalid syntax:",rule))
  }
  return( list(ok=res$ok, parsed=substr(rule,1,res$pos) ) )
}

SetAction<-function(genE, rule.id, action){
  #TODO:  ( expression?)
  #TODO: refactor using switch?
  if(!("genE" %in% class(genE))){ stop("first argument not a peg parser")}  
  if( rule.id %in% RuleIds(genE)){
    if(class(action)=="character"){
      action<-paste("function(v){",action,"}")
      genE$pegE$.ACTION[[rule.id]]<-eval(parse(text=action))  
      return(TRUE)
    } else if (class(action)=="function"){
      genE$pegE$.ACTION[[rule.id]]<-action       
      return(TRUE)
    }
    stop("cannot set action: invalid action")
    return(FALSE)
  } else {
    stop("cannot set action: invalid rule identifier")
  }
}

SetDescription<-function(genE, rule.id, description){
  if(!("genE" %in% class(genE))){ stop("first argument not a peg parser")}  
  if( rule.id %in% RuleIds(genE)){
    genE$pegE$.RULE_DESCRIPT[[rule.id]]<-description
    return(TRUE)
  } else {
    stop("cannot add description: invalid rule identifier")
  }
}

GetDescription<-function(genE, rule.id){
  if(!("genE" %in% class(genE))){ stop("first argument not a peg parser")}  
  if( rule.id %in% RuleIds(genE)){
    return(genE$pegE$.RULE_DESCRIPT[[rule.id]])
  } else {
    stop("cannot add description: invalid rule identifier")
  }
  
}


DeleteRule<-function(genE, rule.id){
  #delete rule 
  if(!("genE" %in% class(genE))){ stop("first argument not a peg parser")}  
  genE$pegE$.SOURCE.RULES[[rule.id]]<-NULL
  genE$pegE$.ACTION[[rule.id]]<-NULL
  genE$pegE$.RULE_DESCRIPT[[rule.id]]<-NULL
  rm(list=rule.id, envir=genE$pegE)    
}

RuleIds<-function(genE){
  if(!("genE" %in% class(genE))){ stop("argument not a peg parser")}
  ls(envir=genE$pegE)->tmp
  if( any(grepl("atom.",tmp) ) ){
    tmp<-tmp[-grep("atom.",tmp)]    
  }
  tmp
}

GetPegE<-function(genE){
  if(!("genE" %in% class(genE))){ stop("first argument not a generator")}  
  genE$pegE
}

#invoke run parse
Parse<-function(genE, rule.id=rule.id, arg=arg, exe=F, debugTree=F){
  if(!("genE" %in% class(genE))){ stop("first argument not a peg parser")}  
  if( !( rule.id %in% RuleIds(genE) ) ){stop("cannot parse: invalid rule identifier")}
  genE$pegE$.DEBUG.NODE<-debugTree
  genE$pegE[[rule.id]](arg)->res
  if(!"list" %in% (class(res)) ){ stop("Bad Action Rule: resulting value is not a list")}
  res$Call<-list(rule.id=rule.id, arg=arg)
  res$options<-list(exe=exe, debugTree=debugTree)
  class(res)<-c("PEGResult",class(res))
  res
}

print.PEGResult<-function(res){
  if(!("PEGResult" %in% class(res))){ stop("Argument not a peg parsing result")} 
  cat(paste0("Call:", paste0(res$Call$rule.id, "(",res$Call$arg,")\n")  ))
  cat(paste("Options:", "Apply Actions=",res$options$exe,"Make Tree=",res$options$debugTree,"\n"))
  cat(paste("Status:", ifelse(res$ok,"Succeeded","Failed\n")))
  cat(paste("Processed:", res$pos, "out of", length(res$arg), "\n"))
  val<-paste(res$val,collapse=",")
  cat(paste("Evaluates to: list(", val, ")\n"))
}

Value<-function(res){
  if(!("PEGResult" %in% class(res))){ stop("Argument not a peg parsing result")} 
  return(res$value) 
}

DisplayTree<-function(res){
  if( is.null(res$debugNode) ){
    stop("Tree option not set, rerun parse with option debugTree=True")
  }
  node.print<-function(n, indent=1){
    s<-paste(rep("",indent),collapse=" ")
    arg<-n$data$consumes$t2 #paste(n$data$consumes, collapse="")
    s<-paste(s,"|__ ", n$data$name, "(",arg , ") = list(", n$data$value ," )\n") 
    cat(s)
    if(length(n$children)>0){
      for(child in n$children){
        node.print(child, indent+3)
      }
    }
  }
  node.print(res$debugNode)
}


plot.PEGResult<-function(res, bg = ifelse(match(par("bg"), "transparent", 0), "white", par("bg")), border = TRUE, xpad = 1.0, ypad = 2.5, 
                         cex = 1, shadow=TRUE, adj = 0.5, ...){
  if( is.null(res$debugNode) ){
    stop("Tree option not set, rerun parse with option debugTree=True")
  }
  rootNode<-res$debugNode
  #save old settings
  cex=1
  oldcex <- par("cex")
  par(cex = cex)
  adj<-.5
  
  if(!exists("main")){
    main=paste("Parse Tree for node:", rootNode$data$name )
  } 
  
  #set up graph screen
  plot(0, main = main, xlim = c(0, 1), ylim = c(0, 1), xlab = "", ylab = "", type = "n", axes = FALSE)
  #initialized nodes, arrows, boxWidths
  nodes<-data.frame()
  arrows<-c()
  boxWidths<-c(0) #actually might be better to call level width
  boxHeights<-0
  xp<-strwidth("m") # a padding
  
  oval<-function(xl, yb, xr, yt,col = bg, border = border,...){
    r<-(yt[1]-yb[1])/2
    n<-20
    theta<-seq(from=-pi/2,to=pi/2, length.out=n)
    xcr<-r*cos(theta)
    ycr<-r*sin(theta)
    for(i in 1:length(xl)){
      x1<-xcr+xr[i]-r
      x2<--xcr+xl[i]+r
      x<-c(x1,x2)
      y1<-ycr+r+yb[i]
      y2<--ycr+r+yb[i]
      y<-c(y1,y2)
      polygon(x,y, col=col, border=border)
    }
  }
  
  traverseOrd<-function(nn, level, pos, width){ #width is the full width 
    nodes<<-rbind(nodes, data.frame(id=nn$id, pos=pos, level=level, text=nn$data$name))
    boxWidths[level+1]<<-max(boxWidths[level+1], strwidth(nn$data$name)+xp) #update box width
    boxHeights<<-max(boxHeights, strheight(nn$data$name)) #update box height
    i<-0
    n<-length(nn$children)
    if(n==0){ return(NULL) }
    if(n==1){ 
      delta<-width/2
      childWidth<-width
    } else {
      delta<-width/(n-1)
      childWidth<-width/n #something slightly smaller than delta
    }
    childLevel<-level+1
    boxWidths<<-c(boxWidths,0) #extend the boxWidths to accommadate the kids
    i<-0
    for(child in nn$children){
      childPos<-delta*i - width/2 + pos
      arrows<<-rbind(arrows, matrix(c(nn$id, child$id),1,2) ) #maybe we should record level position instead
      traverseOrd(child, childLevel, childPos, childWidth)
      i<-i+1
    }
  }
  traverseOrd(rootNode, 0, 0, 2) #do the traveral
  nodes$dbpos<-nodes$pos
  nodes$pos<-sapply(nodes$pos, function(x){which(x==sort(unique(nodes$pos)))})
  #now we plot the results
  #compute the x for each box (recall level gives the x position)
  #we compute the x-center of a node by x[level+1]
  x<-boxWidths+3*xp
  x<-cumsum(x)
  xl<-x-(xp+boxWidths)-2*xp
  xc<-xl+boxWidths/2
  xr<-xl+boxWidths
  nodes$xl<-xl[nodes$level+1]
  nodes$xc<-xc[nodes$level+1]
  nodes$xr<-xr[nodes$level+1] 
  #we compute the y-center of a node by y[pos]
  y<-nodes$pos
  max(y)->noOfRows
  delta<-1.0/noOfRows
  nodes$yt<-delta*(nodes$pos) + boxHeights*.7
  nodes$yc<-delta*(nodes$pos) 
  nodes$yb<-delta*(nodes$pos) -  boxHeights*.7
  #   
  #rect(nodes$xl, nodes$yb, nodes$xr, nodes$yt,col = bg, border = border)
  if(shadow==TRUE){
    xpp<-xp/4
    oval(nodes$xl+xpp, nodes$yb-xpp, nodes$xr+xpp, nodes$yt-xpp, col = "gray", border = FALSE)    
  }
  oval(nodes$xl, nodes$yb, nodes$xr, nodes$yt,col = bg, border = border)
  
  #   #compute the y for each box (recall pos gives the y position)
  #plot the boxes
  adj<-.5
  args <- list(x = nodes$xc, y = nodes$yc, labels = nodes$text,  adj = adj, col = ifelse(colSums(col2rgb(bg) * c(1, 1.4, 0.6)) < 350, "white", "black"))
  args <- modifyList(args, list(...))
  do.call(text, args)
  par(cex = oldcex)
  
  cubic<-function(p.x, p.y, q.x, q.y, n=20) {
    print(paste(p.x,p.y,q.x,q.y))
    f<-function(x){
      (x^3)/3 -(p.x + q.x) * (x^2)/2 + q.x*p.x*x
    }
    K<-( q.y-p.y )/( f(q.x)-f(p.x) )
    C<-p.y-K*f(p.x)
    x<-seq(from=p.x, to=q.x, length=n)
    y<-K*f(x)+C
    df<-data.frame(x=x,y=y) 
    lines(df$x, df$y)
  }
  
  for(i in 1:nrow(arrows)){
    id.from<-arrows[i,1]
    id.to<-arrows[i,2]
    r1<-subset(nodes, nodes$id==id.from)
    r2<-subset(nodes, nodes$id==id.to)
    p.x<-xr[r1$level+1]
    q.x<-xl[r2$level+1]
    p.y<-delta*r1$pos
    q.y<-delta*r2$pos
    cubic(p.x, p.y, q.x, q.y)
  }
  
}

#todo
#print peg #print(pegE)
#print history of run #parse(pegE), run(pegE)
#print peg(parse) history #history(pegE.result)
#print peg(parse) final state 
#add rule action to generator addRule(pegE, rule)
#print rule name/ description describe(pegE)
#print all rules: rules(pegE)
#summary(pegE)
#rule.ids(pegE)
#plot(pegE)
#actions(pegE)
#ruleforms(pegE)
#str


#def open expression close true
#def and true
#def not true
