
#PEG COMPONENTS
#Note the following have explicit reference to pegE
# s.sequence
# s.first
# s.not
# opt.01
# opt.0x
# opt.1x
# s.and
#however the above only are invoked via a peg.name, which is valid only for identifiers in pegE
# debug.Node


# if f is a peg.name, look it up and return 
# ow. return f (which should be either a function or the value of a atom)
# get.fn<-function(f{
#   if("peg.name" %in% class(f)){
#     if(exists(f)){ f<-get(f)}}
#   f
# }

#source("node.R")
include.sComponents<-function(pegE, envS=parent.frame() ){
  
  DEVEL.DEBUG<-envS$DEVEL.DEBUG
  
  
  envS$debuggin.peg<-function(){
    #   return(TRUE)
    return(exists("PEG.DEBUG.L1", envir=env)) 
  }
  
  
  # envS$debug.Node<-function(){ #this is for the user, !!!todo: debug.node for generator
  # #   return(TRUE)
  #   return( exists(".DEBUG.NODE",envir=pegE))
  # }
  
  
  envS$s.atom<-function(x){ # (x) terminal symbol 
    h<-function(input, exe=TRUE,  p=1){
      n<-nchar(x)-1
      if(envS$DEVEL.DEBUG){
        cat("atom.",x," input=",input," p=",p,"\n") ###good for debugging      
      }    
      if( substring(input,p,p+n)==x){
        val=list(atom=x)
        return( list(ok=TRUE,pos=n+1, val=val  ) )
      }else{
        return(list(ok=FALSE,pos=0))
      }
    }
    class(h)<-c("pa", "pe",class(h))
    
    h
    #   fn<-memoize(h)
    #   fn
  }
  
  envS$s.range<-function(begChar,endChar){
    h<-function(input, exe=TRUE,  p=1){
      theChar<-substring(input,p,p)
      if(begChar<=theChar & theChar<=endChar ){
        val=list(atom=theChar)
        return (list(ok=TRUE, pos=1, val=val))
      }else{
        return(list(ok=FALSE,pos=0))
      }
    }
    class(h)<-c("pa", "pe",class(h))
    h
    #   fn<-memoize(h)
    #   fn  
  }
  
  envS$s.dot<-function(input, exe=TRUE,  p=1){
    if(envS$DEVEL.DEBUG){
      cat("s.dot"," input=",input," p=",p,"\n") ###good for debugging      
    }    
    if(p>nchar(input)){
      return(list(ok=FALSE, pos=0, val=list("") ) )
    }
    val<-substring(input, p, p)
    return( list(ok=TRUE, pos=1, val=list(val) ) )
  }
  #assign("class(s.dot)", c("pa","pe","function")) #kluge, probably want to change this later
  
  # s.eps<-function(input, exe=TRUE,  p=1){
  #   return( list(ok=TRUE, pos=0, val=""))
  # }
  
  #s.sequence, s.first, s.not, 
  
  #' Combines a sequence of nodes 
  #' 
  #' @examples
  #' s.sequence(s.atom('a'),s.atom('b'))
  #' @return a list containg a list containg an ok, pos, val, and an optional debugNodes 
  envS$s.sequence<-function(...){ 
    lf<-list(...)
    h<-function(input, exe=TRUE,  p=1){  
      if(envS$DEVEL.DEBUG){
        cat("sequence: input=",input," p=",p,"\n") ###good for debugging      
      } 
      mn<-0
      val=list()
      if(pegE$.DEBUG.NODE==T){ #create a container for a list of debug nodes of the children
        d.node<-list()
      }
      for(f in lf){
        if("peg.name" %in% class(f)){ 
          if(exists(f,envir=pegE)){ 
            f<-get(f,envir=pegE)
          } else {
            stop(paste("missing symbol:", f , "(rule missing quotes?)" ), call.=FALSE )
          }
        }
        #f<-get.fn(f)
        res<-f(input,  exe, p+mn)
        if(res$ok==FALSE){
          return(list(ok=FALSE,pos=0, val=list() )) #this line is the essential difference between sequenceNode and sequence
        }
        mn<-mn+res$pos
        val<-c(val,res$val)
        if(pegE$.DEBUG.NODE==T & !is.null(res$debugNode)){ 
          #d.node<-c(d.node, list(res$debugNode) ) #c(list(), list(a))=list(a)
          d.node<-c(d.node, res$debugNode ) #c(list(), list(a))=list(a) #!!!
        }     
      }
      if(pegE$.DEBUG.NODE==T){ 
        return(list(ok=TRUE, pos=mn, val=val, debugNode=d.node)) #append that container onto debugNode of this
      } else {
        return(list(ok=TRUE, pos=mn, val=val))      
      }    
    }
    class(h)<-c("pe",class(h))
    h
    #   fn<-memoize(h)
    #   fn
  }
  
  
  
  
  ###
  ###
  envS$s.first<-function(...){ #(f > g) or
    lf<-list(...)
    h<-function(input, exe=TRUE,  p=1){  
      if(envS$DEVEL.DEBUG){
        cat("first: input=",input," p=",p,"\n") ###good for debugging      
      } 
      val=list()
      for(f in lf){
        #if("peg.name" %in% class(f)){ if(exists(f)){ f<-get(f)}}
        if("peg.name" %in% class(f)){ if(exists(f,envir=pegE)){ f<-get(f,envir=pegE)}}
        res<-f(input, exe,  p)
        if(res$ok==TRUE){
          return(res)
        }
      }
      return(list(ok=FALSE, pos=0, val=list() ))
    }
    class(h)<-c("pe",class(h))
    h
    #   fn<-memoize(h)
    #   fn
  }
  
  ###
  
  # s.or<-function(f,g){ #(f / g) prioritized choice
  #   h<-function(input, exe=TRUE,  p=1){
  #     res1<-f(input, exe,  p)
  #     if(res1$ok){
  #       val<-res1$val
  #       return(list(ok=TRUE,pos=res1$pos,val=val))
  #     } 
  #     res2<-g(input, exe,  p)
  #     if(res2$ok==TRUE){
  #       val<-res2$val
  #       return(list(ok=TRUE,pos=res2$pos,val=val))     
  #     } else
  #       return(list(ok=FALSE, pos=0, val=list() ))
  #   }
  #   class(h)<-c("pe",class(h))
  #   h
  #   #   fn<-memoize(h)
  #   #   fn
  # }
  
  # "!.pe"<-function(f){ #not before
  #   h<-function(input, exe=TRUE,  p=1){
  #     if(debuggin.peg()){
  #       cat("negate: input=",input," p=",p,"\n") ###good for debugging      
  #     }    
  #     #     if( p > length(input,  exe))
  #     #       return(list(ok=FALSE, pos=0, val=list() ))
  #     exe<-FALSE
  #     res<-f(input, exe,  p)
  #     if(res$ok==TRUE){
  #       return(list(ok=FALSE,pos=0, val=list() ))
  #     } else {
  #       return(list(ok=TRUE,pos=0, val=list() ))
  #     }
  #   }
  #   class(h)<-c("pe",class(h))
  #   h
  #   #   fn<-memoize(h)
  #   #   fn
  # }
  
  envS$s.not<-function(f){ #not before
    h<-function(input, exe=TRUE,  p=1){ #not before
      if(envS$DEVEL.DEBUG){
        cat("negate: input=",input," p=",p,"\n") ###good for debugging      
      }    
      exe<-FALSE
      #if("peg.name" %in% class(f)){ if(exists(f)){ f<-get(f)}}
      if("peg.name" %in% class(f)){ if(exists(f,envir=pegE)){ f<-get(f,envir=pegE)}}
      res<-f(input,  FALSE, p)
      #print(res)
      if(res$ok){
        return(list(ok=FALSE,pos=0, val=list() ))
      } else {
        return(list(ok=TRUE,pos=0, val=list() ))
      }
    }
    class(h)<-c("pe",class(h))
    h
    #   fn<-memoize(h)
    #   fn
  }
  
  envS$opt.01<-function(f){ # [f] (optional: one possible occurance of f)
    h<-function(input, exe=TRUE,  p=1){
      #cat("optional")
      #if("peg.name" %in% class(f)){ if(exists(f)){ f<-get(f)}}
      if("peg.name" %in% class(f)){ if(exists(f,envir=pegE)){ f<-get(f,envir=pegE)}}
      res<-f(input, exe,  p)
      if(res$ok){
        #       val=res$val
        #       return(list(ok=TRUE,pos=res$pos, val=val))
        return(res)
      }
      return(list(ok=TRUE,pos=0,val=list() ))
    }  
    class(h)<-c("pe",class(h))
    h
    #   fn<-memoize(h)
    #   fn
  } 
  
  
  # printRes<-function(no, res){
  #   cat("\nres", no, ": ok=", res$ok, " pos=",res$pos, "val=",  "\n")
  #   print(res$val)
  # }
  # 
  
  
  
  envS$opt.0x<-function(f){ # f* :zero more repitions
    h<-function(input, exe=TRUE,  p=1){
      ok<-TRUE
      m<-0
      val<-c()
      if(pegE$.DEBUG.NODE==T){ 
        d.node<-list()
      }
      while(ok){
        #if("peg.name" %in% class(f)){ if(exists(f)){ f<-get(f)}}
        if("peg.name" %in% class(f)){ if(exists(f,envir=pegE)){ f<-get(f,envir=pegE)}}
        res<-f(input,  exe,  p+m)
        #cat("m=",m," p+m=",p+m, "\n")
        #printRes(m,res)
        ok<-res$ok
        if(ok){
          m<-m+res$pos
          val<-c(val,res$val)        
          if(pegE$.DEBUG.NODE==T){ 
            d.node<-c(d.node, res$debugNode)
          }     
        }
      }
      #val<-paste(val,collapse="")
      return(list(ok=TRUE,pos=m, val=val))
      if(pegE$.DEBUG.NODE==T & length(d.node)>0) { 
        return(list(ok=TRUE, pos=m, val=val, debugNode=d.node))
      } else {
        return(list(ok=TRUE, pos=m, val=val))      
      }    
    }  
    class(h)<-c("pe",class(h))
    h
    #   fn<-memoize(h)
    #   fn
  }
  
  
  envS$opt.1x<-function(f){ # f* :one or  more repitions
    h<-function(input, exe=TRUE,  p=1){
      #if("peg.name" %in% class(f)){ if(exists(f)){ f<-get(f)}}
      if("peg.name" %in% class(f)){ if(exists(f,envir=pegE)){ f<-get(f,envir=pegE)}}
      res1<-f(input, exe,  p)
      if(res1$ok){
        #val1<-res1$val
        res2<-envS$opt.0x(f)( input, exe, p+res1$pos )
        m<-res1$pos+res2$pos
        #val<-c(val1,res2$val)  
        val<-c(res1$val,res2$val)     
        if(pegE$.DEBUG.NODE==T ) { 
          d.node=c(res1$debugNode,res2$debugNode)
          return(list(ok=TRUE, pos=m, val=val, debugNode=d.node))
        } else {
          return(list(ok=TRUE, pos=m, val=val))      
        }    
      }
      return(list(ok=FALSE,pos=0, pos=list() ))
    }  
    class(h)<-c("pe",class(h))
    h
    #   fn<-memoize(h)
    #   fn
  }
  
  envS$s.and<-function(f){ # f* :lookahead or "and" or &
    h<-function(input, exe=TRUE,  p=1){
      exe<-FALSE
      #if("peg.name" %in% class(f)){ if(exists(f)){ f<-get(f)}}
      if("peg.name" %in% class(f)){ if(exists(f,envir=pegE)){ f<-get(f,envir=pegE)}}
      res<-f(input, exe,  p)
      val<-res$val
      if(res$ok){
        return(list(ok=TRUE,pos=0, val=list() ))
      }
      return(list(ok=FALSE,pos=0, val=list() ))
    }  
    class(h)<-c("pe",class(h))
    h
    #   fn<-memoize(h)
    #   fn
  }
  
  #this makes the atom, even if it previously existed!!!
  envS$mk.atom<-function(x, y=NULL, env=envS){ #env=parent.frame() ){ #used in literal, unitTest1.r, unitTest2.r, test1.r, test2.r
    if(is.null(y)){
      y<-x
    }
    #cat("x=",x)
    tmp<-envS$s.atom(x)
    #print(tmp)
    name<-paste("atom",y,sep=".")
    assign(name, tmp ,env )#, envir=.GlobalEnv) #, envir = pegE) #.GlobalEnv)
    name
  }
  
  envS$forget.all<-function(){
    ll<-ls()
    z<-sapply(ll,function(x){ forget(get(x))})
  }
  
  envS$delete.all.atoms<-function(){ #used in unitTest2.r
    ll<-ls(pattern="^atom\\.*")
    rm(ll,env=parent.frame())
  }
  
  envS$peg.parse<-function(xx){ #used in unitTest1.r, test1.r, test2.r
    n<-rule.S(xx)
    n$ok
  }
  
} #end sComponents

include.gComponents<-function(pegE, envG=parent.frame() ){
  
  DEVEL.DEBUG<-envG$DEVEL.DEBUG
  
  envG$"/.pe"<-function(f,g){ #(f / g) prioritized choice
    h<-function(input, exe=TRUE,  p=1){
      if(DEVEL.DEBUG){
        cat("disjuction: input=",input," p=",p,"\n") ###good for debugging      
      }    
      #if("peg.name" %in% class(f)){ if(exists(f)){ f<-get(f)}}
      if("peg.name" %in% class(f)){ if(exists(f,envir=pegE)){ f<-get(f,envir=pegE)}}
      res1<-f(input, exe,  p)
      if(res1$ok){
        #val<-res1$val
        #return(list(ok=TRUE,pos=res1$pos,val=val))
        return(res1)
      } 
      #if("peg.name" %in% class(g)){ if(exists(g)){ f<-get(g)}}
      if("peg.name" %in% class(g)){ if(exists(g,envir=pegE)){ g<-get(g,envir=pegE)}}
      res2<-g(input, exe,  p)
      if(res2$ok==TRUE){
        #val<-res2$val
        #return(list(ok=TRUE,pos=res2$pos,val=val))
        return(res2)
      } else
        return(list(ok=FALSE, pos=0, val=list() ))
    }
    class(h)<-c("pe",class(h))
    h
    #   fn<-memoize(h)
    #   fn
  }
  
  envG$"+.pe"<-function(f,g){ #(f > g) sequence
    h<-function(input, exe=TRUE,  p=1){
      mn<-0
      if(DEVEL.DEBUG){
        cat("conjuction: input=",input," p=",p,"\n") ###good for debugging      
      }
      #f<-get.fn(f)
      if("peg.name" %in% class(f)){ if(exists(f,envir=pegE)){ f<-get(f,envir=pegE)}}   
      res1<-f(input, exe,  p)
      if(res1$ok){
        #g<-get.fn(g)
        if("peg.name" %in% class(g)){ if(exists(g,envir=pegE)){ g<-get(g,envir=pegE)}}
        res2<-g(input,  exe, p+res1$pos)
        if(res2$ok){
          val=c(res1$val,res2$val) 
          pos=res1$pos+res2$pos
          if(pegE$.DEBUG.NODE==T){ 
            #           print("debug.Node")
            debugNode<-c(res1$debugNode, res2$debugNode)
            return(list(ok=TRUE, pos=pos, val=val, debugNode=debugNode ))
          } else {
            #           print("No debug.Node")
            return(list(ok=TRUE, pos=pos, val=val))      
          }    
        }
      }
      return(list(ok=FALSE,pos=0, val=list() ))
    }
    class(h)<-c("pe",class(h))
    h
    #   fn<-memoize(h)
    #   fn
  }
  
  #'Used by generator to parse the user peg. 
  #'That is, the instructions after < generate the corresponding peg 
  #'node is the generator rule (such as SUFFIX, or DEFINITION)
  #'mfn is a list, consisting of generator rule and corresponding actions
  #'
  #'creates a function with arguments
  #'input the user defined rule to be parsed into a peg function
  #'exe flag, if true will execute the user code when applied
  #'p the position where to begin to parse that user rule
  envG$'<.pe'<-function(node, mfn){
    #check on that mfn is  alist with message and function
    #   if(!("function" %in% class(mfn))){
    #     cat"Bad function"
    #     exit()
    #   }   
    #wrap
    
    h<-function(input, exe=TRUE,  p=1){
      mfn.mssg<-mfn[[1]] #rule Generator Rule Name
      mfn.fn<-mfn[[2]] #instructions(generator actions) for this rule
      #     print("node")
      #     print(node)
      #     print("mfn")
      #     print(mfn)
      #     print("input")
      #     print(input)
      res<-node(input, exe,  p) 
      if(length(res$val)>0 & "character" %in% class(res$val)){
        mfm.mssg<-res$val[[1]]
      }
      if(res$ok==FALSE){   
        if(DEVEL.DEBUG){ 
          print(paste("Generator: ",mfn.mssg," Node Rejected: Exiting node: ",sep="=> ")) 
          print(res$val)
        }
        return(res)
      } else { #res$ok=TRUE
        if(DEVEL.DEBUG){
          pp<-p+res$pos
          cat("      pp=",pp,"  :\n")
          print(paste(mfn.mssg, "node value:", sep="=>"))
          tmp<-unlist(res$val)
          cat("      ")
          print(paste(tmp,collapse=", "))
        }    
        if(exe==TRUE  & !is.null(mfn.fn)){
          res$val<-mfn.fn(res$val)
          if( DEVEL.DEBUG ){
            print(paste(mfn.mssg, "action value:", sep="=>"))
            tmp<-unlist(res$val)
            cat("      ")
            print(paste(tmp,collapse=", "))
          }
        }
        if( DEVEL.DEBUG ){
          print(paste(mfn.mssg, "Node Succeeded: Exiting node:", sep="=>"))
        }   
      }
      res
    }
    class(h)<-c("rule",class(node))
    h
  }
  
  
} # gComponents
