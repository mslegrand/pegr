#
# mk.atom.name:  (locally only) by mk.c.a
# mk.c.a: used (locally only) by mk.s.a and mk.l.a
#       :uses mk.atom (only user besides test1,2)
# mk.s.a: used (locally only by) literal
# literal: used by generator to build generator rules, but not used by generator actions! Also used in numeous tests
# mk.l.a : used only by generator rule LITERAL actions   <<-- move to generator?
# mk.prefix: used by generator only : generator rule PREFIX action
# mk.suffix: used by generator only : generator rule SUFFIX action
# mk.rng.a: used by generarot only : generator rule CLASS<-s.seq action

# literal calls {mk.s.a calls{ mk.c.a calls {mk.atom.name, mk.atom calls {s.atom} }  }  } 
# mk.l.a calls {mk.c.a calls {mk.atom.name, mk.atom calls {s.atom} }
# mk.prefix calls { s.not, s.and }
# mk.suffix calls { s.op.0  , s.opt.0x, s.opt.1x}
# mk.rng.a calls { s.range }

# gen_rule:actions 
#        |
#        |____literal # makes the atom in the parent env (which is the genE)
#                |_____mk.s.a
#                        |_____mk.c.a
#                                 |______mk.atom.name
#                                 |______mk.atom
#                                            |_____s.atom

#gen_LITERAL:action
#        |__________mk.l.a # makes the atom in pegE
#                     |_______mk.c.a
#                                 |______mk.atom.name
#                                 |______mk.atom
#                                            |_____s.atom

# test1 calls: sComponents:s.atom  sComponents:mk.atom
# test2 calls: sComponents:s.atom  sComponent:mk.atom
# test3 calls: literal:literal
# test4 calls:
# test5 calls:
# test6 calls:

#todo:
# rename mk.l.a to mk.pegE.Literal with (pegE specified as an arg?) and move to file pegELiteral
# move literal to file called genELiteral
# have both pegELiteral and genEliteral import their own env literal env.
# import as appropriate into generator, test3

#   note: mk.l.a is executed inside genE, and genE contains a pegE (at least for now)
#
# The pegE is populated by the actions of the genE rule set, so if we make pegE an array, then we
# need to do a execq(..., envir=pegE), for genE? (or can we just add the rules willy nilly?)


#PEG LITERAL BUILDER
include.literal<-function(pegE, envL=parent.frame() ){
  special.characters<-c(
    empty="",
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