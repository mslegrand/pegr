

# scomponent
  # debuggin.peg: not used?

  # s.range: test4(1); literal(1); generator(2) 
  #           called by { literal:mk.rng.a called by { generator:CLASS:action }, test4 }
  # s.dot: test2(12); generator
  # s.atom: used by test2 (2);  sComponents:mk.atom 
  # mk.atom: used by test1 (2), test2(5) and literal <- can we replace this in Test1 and 2?

  # delete.all.atoms: used only in test2 <- remove this!!
  # peg.parse: used only in test1(4) <- remove this


# Can we change this to:
# 1. s.components as a list, s.com$s.range<-function(..)..
# 2. add this to the envS by using something like
# clone_env <- function(env, parent = parent.env(env)) {
#   list2env(as.list(env), parent = parent)
# }
# this produces a new env from the parent together with the list
# It does not add onto the existing env!
# 
# One way is to attach do the work and detach, issue may arise if an
# exception is thrown.
# 
# Another approach is to use local{ ..., env=}
# So to use scomponets inside a function, we might try
# local( 
# all our code
# , envir= genE
# )
# 
#    evalq(expr, envir=peg)
# or 
# expr <- substitute(x)
#  eval(expr, latex_env(expr))


include.sComponents<-function(pegE, envS=parent.frame() ){
  
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
#     class(h)<-c("pa", "pe",class(h))
#     h
    fn<-memoise::memoize(h)
    class(fn)<-c("pa", "pe",class(fn))
    fn  
  }
  
  envS$s.dot<-function(input, exe=TRUE,  p=1){
    if(p>nchar(input)){
      return(list(ok=FALSE, pos=0, val=list("") ) )
    }
    val<-substring(input, p, p)
    return( list(ok=TRUE, pos=1, val=list(atom=val) ) )
  }
  
  #this is used by envS$mk.atom (and twice in test2.r ) 
  envS$s.atom<-function(x){ # (x) terminal symbol 
    h<-function(input, exe=TRUE,  p=1){
      n<-nchar(x)-1 #for example if x=='a', then nchar('a')==1, so n=0
      if( substring(input,p,p+n)==x){
        val=list(atom=x)
        return( list(ok=TRUE,pos=n+1, val=val  ) ) #and if x='a' then pos=1
      }else{
        return(list(ok=FALSE,pos=0))
      }
    }
#     class(h)<-c("pa", "pe",class(h))    
#     h
    fn<-memoise::memoize(h)
    class(fn)<-c("pa", "pe",class(fn))
    fn
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

