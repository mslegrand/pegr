
include.sConnectives<-function(pegE, envS=parent.frame() ){
 
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
  
  #s.and in literal, test1, 
  
  getSafeFn<-function(f){
    if("peg.name" %in% class(f)){
      if(exists(f,envir=pegE)){ 
        f<-get(f,envir=pegE)
      } else {
        stop(paste("missing symbol:", f , "(rule missing quotes?)" ), call.=FALSE )
      }
    }
    f
  }         
  
  
  #' Combines a sequence of nodes 
  #' 
  #' @examples
     #' s.sequence(s.atom('a'),s.atom('b'))
     #' @return a list containg a list containg an ok, pos, val, and an optional debugNodes 
     envS$s.sequence<-function(...){ 
       lf<-list(...)
       h<-function(input, exe=TRUE,  p=1){  
         mn<-0
         val=list()
         if(pegE$.RECORD.NODE==T){ #create a container for a list of debug nodes of the children
           d.node<-list()
         }
         for(f in lf){
           
           # grab f
           f<-getSafeFn(f)           
           
           # execute f
           res<-f(input,  exe, p+mn)
           
           #process
           if(res$ok==FALSE){
             return(list(ok=FALSE,pos=0, val=list() )) #this line is the essential difference between sequenceNode and sequence
           }
           mn<-mn+res$pos
           val<-c(val,res$val)
           if(pegE$.RECORD.NODE==T & !is.null(res$debugNode)){ 
             d.node<-c(d.node, res$debugNode ) #c(list(), list(a))=list(a) #!!!
           }     
         }
         if(pegE$.RECORD.NODE==T){ 
           return(list(ok=TRUE, pos=mn, val=val, debugNode=d.node)) #append that container onto debugNode of this
         } else {
           return(list(ok=TRUE, pos=mn, val=val))      
         }    
       }
#        class(h)<-c("pe",class(h))
#        h
       fn<-memoize(h)
       class(fn)<-c("pe",class(fn))
       fn
     }
     
     ###
     ###
     envS$s.first<-function(...){ #(f > g) or
       lf<-list(...)
       h<-function(input, exe=TRUE,  p=1){  
         val=list()
         for(f in lf){
           if("peg.name" %in% class(f)){ if(exists(f,envir=pegE)){ f<-get(f,envir=pegE)}}
           # grab f
           f<-getSafeFn(f)

           # execute f
           res<-f(input, exe,  p)
           
           #process
           if(res$ok==TRUE){
             return(res)
           }
         }
         return(list(ok=FALSE, pos=0, val=list() ))
       }
#        class(h)<-c("pe",class(h))
#        h
       fn<-memoize(h)
       class(fn)<-c("pe",class(fn))
       fn
     }
     
     
     envS$s.not<-function(f){ #not before
       h<-function(input, exe=TRUE,  p=1){ #not before
         exe<-FALSE #lookahead does not exe
         
         # grab
         f<-getSafeFn(f)
         
         # execute f      
         res<-f(input,  FALSE, p)
         
         #process
         if(res$ok){
           return(list(ok=FALSE,pos=0, val=list() ))
         } else {
           return(list(ok=TRUE,pos=0, val=list() ))
         }
       }
#        class(h)<-c("pe",class(h))
#        h
       fn<-memoize(h)
       class(fn)<-c("pe",class(fn))
       fn
     }
  
    envS$s.and<-function(f){ # f* :lookahead or "and" or &
      h<-function(input, exe=TRUE,  p=1){
        
        exe<-FALSE #lookahead, do not exe
        
        # grab f
        f<-getSafeFn(f)
        
        # execute f      
        res<-f(input, exe,  p)
        
        #process
        val<-res$val
        if(res$ok){
          return(list(ok=TRUE,pos=0, val=list() ))
        }
        return(list(ok=FALSE,pos=0, val=list() ))
      }  
#       class(h)<-c("pe",class(h))
#       h
      fn<-memoize(h)
      class(fn)<-c("pe",class(fn))
      fn
    }
     
     envS$opt.01<-function(f){ # [f] (optional: one possible occurance of f)
       h<-function(input, exe=TRUE,  p=1){         
         # grab f
         f<-getSafeFn(f)
         
         # execute f      
         res<-f(input, exe,  p)
         
         #process
         if(res$ok){
           #       val=res$val
           #       return(list(ok=TRUE,pos=res$pos, val=val))
           return(res)
         }
         return(list(ok=TRUE,pos=0,val=list() ))
       }  
#        class(h)<-c("pe",class(h))
#        h
       fn<-memoize(h)
       class(fn)<-c("pe",class(fn))
       fn
     } 
     
     envS$opt.0x<-function(f){ # f* :zero more repititions
       h<-function(input, exe=TRUE,  p=1){
         ok<-TRUE
         m<-0
         val<-c()
         if(pegE$.RECORD.NODE==T){ 
           d.node<-list()
         }
         while(ok){
           # grab f
           f<-getSafeFn(f)
           
           # execute f      
           res<-f(input,  exe,  p+m)
           
           #processs
           ok<-res$ok
           if(ok){
             m<-m+res$pos
             val<-c(val,res$val)        
             if(pegE$.RECORD.NODE==T){ 
               d.node<-c(d.node, res$debugNode)
             }     
           }
         }
         return(list(ok=TRUE,pos=m, val=val))
         if(pegE$.RECORD.NODE==T & length(d.node)>0) { 
           return(list(ok=TRUE, pos=m, val=val, debugNode=d.node))
         } else {
           return(list(ok=TRUE, pos=m, val=val))      
         }    
       }  
#        class(h)<-c("pe",class(h))
#        h
       fn<-memoize(h)
       class(fn)<-c("pe",class(fn))
       fn
     }
     
     
     envS$opt.1x<-function(f){ # f* :one or  more repitions
       h<-function(input, exe=TRUE,  p=1){
         # grab f
         f<-getSafeFn(f)
         
         # execute f      
         res1<-f(input, exe,  p)
         
         #process
         if(res1$ok){
           res2<-envS$opt.0x(f)( input, exe, p+res1$pos )
           m<-res1$pos+res2$pos 
           val<-c(res1$val,res2$val)     
           if(pegE$.RECORD.NODE==T ) { 
             d.node=c(res1$debugNode,res2$debugNode)
             return(list(ok=TRUE, pos=m, val=val, debugNode=d.node))
           } else {
             return(list(ok=TRUE, pos=m, val=val))      
           }    
         }
         return(list(ok=FALSE,pos=0, pos=list() ))
       }  
#        class(h)<-c("pe",class(h))
#        h
       fn<-memoize(h)
       class(fn)<-c("pe",class(fn))
       fn
     }
     

}

