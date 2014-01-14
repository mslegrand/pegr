
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

include.sComponents<-function(pegE, envS=parent.frame() ){
  
  DEVEL.DEBUG<-envS$DEVEL.DEBUG
  
  
  envS$debuggin.peg<-function(){
    #   return(TRUE)
    return(exists("PEG.DEBUG.L1", envir=env)) 
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
  
  #this is used by envS$mk.atom (and twice in test2.r ) 
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
  
    
  #this is used only by literal mk.c.a, and in test1 and test2.
  #this makes the atom, even if it previously existed!!!
  envS$mk.atom<-function(x, y=NULL, env=envS){ #env=parent.frame() ){ #used in literal, unitTest1.r, unitTest2.r, test1.r, test2.r
    if(is.null(y)){
      y<-x
    }
    #cat("x=",x)
    tmp<-envS$s.atom(x)
    name<-paste("atom",y,sep=".")
    assign(name, tmp ,env )#, envir=.GlobalEnv) #, envir = pegE) #.GlobalEnv)
    name
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

