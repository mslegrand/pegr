#PEG LITERAL BUILDER
include.literal<-function(pegE, envL=parent.frame() ){
  special.characters<-c(
    space=" ",
    bang="!",
    literal2="\"",
    pound="#",
    dollar="$",
    percent="%",
    ampersand="&",
    literal1="\'",
    leftParen="(",
    rightParen=")",
    star="*",
    plus="+",
    comma=",",
    dot=".",
    slash="/",
    colon=":",
    semicolon=";",
    langle="<",
    equal="=",
    ranlge=">",
    question="?",
    at="@",
    leftbracket="[",
    backslash="\\",
    rightbracket="]",
    hat="hat",
    underscore="_",
    backtick="`",
    leftCurly="{",
    verticle="|",
    rightCurly="}",
    tilda="~",
    dash="-"
  )
  
  
  mk.atom.name<-function(alist){
    to.name<-function(token){
      if(token %in% special.characters){
        token.name<-names(special.characters)[match(token,special.characters)]
      } else{
        token.name<-token
      }
      return(token.name)
    }
    aName<-lapply(alist,to.name)
    aName<-paste(aName, collapse=".")
    return(aName)
  }
  
  #mk.atom.from.literal.string
  mk.s.a<-function(x, env){ # takes a string an makes an atom from it. 
    v<-mk.c.a(strsplit(x,""), env)
    return(v)
  }
  
  envL$mk.l.a<-function(v){ #used soley by LITERAL in generator::new.generator
    v[[length(v)]]<-NULL
    v[[1]]<-NULL
    env<-pegE
    aName<-mk.c.a(v, env)
    v<-get(aName,env)
    return(v)
  }
  
  # REVISIT THIS FUNCTION, .GLOBALENV MAY NOT BE CORRECT!!!!
  mk.c.a<-function(v, env){ #TODO: specify either genE or pegE, check if exists in gen/peg and if not create
    y<-unlist(v)
    name<-mk.atom.name(y)
    v<-paste(y,collapse="")
    aName<-paste("atom",name,sep=".")
    if(! aName %in% ls(envir=env)){ # if we use ls(envir = pegE) or ls(), then literal for generator fails at atom.\n
      envL$mk.atom(v,name,env) #returns the name of the atom, but not the atom    
    }
    return(aName)  
  }
  
  envL$mk.rng.a<-function(v){ #used in generator.create
    #looks like [x-x] or [x]
    a<-v[[2]] #get 2nd char
    #get 2nd to last char
    z<-v[[length(v)-1]]
    #create range function
    fn<-envL$s.range(a,z)
    fn
  }
  
  
  #envL$mk.eatspace<-function(v){ list() }
  
  envL$literal<-function(v){ #takes a string and returns an atom, use by generator
    env<-parent.frame()
    name<-mk.s.a(v,env) 
    get(name, envir=env)
  }
  
  envL$mk.suffix<-function(v){ #:generator
    last<-v[[length(v)]]
    fn<-v[[1]]
    if(length(v)==1){ #or we could use if(class(last)!=char)
      return(fn)
    }
    if(last=="?") {
      return(envL$opt.01(fn))
    }
    if(last=="*"){
      return(envL$opt.0x(fn))
    }
    if(last=="+"){
      return(envL$opt.1x(fn))
    }
    return(fn)
  }
  
  envL$mk.prefix<-function(v){ #:generator
    if(length(v)==1){
      return(v[[1]])
    }
    fn<-v[[2]]
    if(v[[1]]=="!"){
      return(envL$s.not(fn))
    }
    if(v[[1]]=="&"){
      return(envL$s.and(fn))
    }
    return(v[[1]])  #shold never get here!!
  }
}