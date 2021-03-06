
include.gConnectives<-function(pegE, envG=parent.frame() ){
  
# contains: 
  # "/.pe"
  # "+.pe"
  # '<.pe'
  
  
  
  envG$"/.pe"<-function(f,g){ #(f / g) prioritized choice
    h<-function(input, exe=TRUE,  p=1){
      if("peg.name" %in% class(f)){ if(exists(f,envir=pegE)){ f<-get(f,envir=pegE)}}
      res1<-f(input, exe,  p)
      if(res1$ok){
        return(res1)
      } 
      if("peg.name" %in% class(g)){ if(exists(g,envir=pegE)){ g<-get(g,envir=pegE)}}
      res2<-g(input, exe,  p)
      if(res2$ok==TRUE){
        return(res2)
      } else
        return(list(ok=FALSE, pos=0, val=list() ))
    }
#      class(h)<-c("pe",class(h))
#      h
    fn<-memoise::memoize(h)
    class(fn)<-c("pe",class(fn))
    fn
  }
  
  envG$"+.pe"<-function(f,g){ #(f > g) sequence
    h<-function(input, exe=TRUE,  p=1){
      mn<-0
      if("peg.name" %in% class(f)){ if(exists(f,envir=pegE)){ f<-get(f,envir=pegE)}}   
      res1<-f(input, exe,  p)
      if(res1$ok){
        if("peg.name" %in% class(g)){ if(exists(g,envir=pegE)){ g<-get(g,envir=pegE)}}
        res2<-g(input,  exe, p+res1$pos)
        if(res2$ok){
          val=c(res1$val,res2$val) 
          pos=res1$pos+res2$pos
          if(pegE$.RECORD.NODE==T){ 
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
#     class(h)<-c("pe",class(h))
#     h
    fn<-memoise::memoize(h)
    class(fn)<-c("pe",class(fn))
    fn
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
      #browser()
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
        return(res)
      } else { #res$ok=TRUE
        if(exe==TRUE  & !is.null(mfn.fn)){
          res$val<-mfn.fn(res$val)
        }
      }
      res
    }
    class(h)<-c("rule",class(node))
    h
  }
  
  
} # gComponents
