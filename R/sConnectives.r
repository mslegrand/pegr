
include.sConnectives<-function(pegE, envS=parent.frame() ){
  
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
           # grab f
           if("peg.name" %in% class(f)){
             if(exists(f,envir=pegE)){ 
               f<-get(f,envir=pegE)
             } else {
               stop(paste("missing symbol:", f , "(rule missing quotes?)" ), call.=FALSE )
             }
           }
           # execute f
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
           if("peg.name" %in% class(f)){ if(exists(f,envir=pegE)){ f<-get(f,envir=pegE)}}
           # grab f
           if("peg.name" %in% class(f)){
             if(exists(f,envir=pegE)){ 
               f<-get(f,envir=pegE)
             } else {
               stop(paste("missing symbol:", f , "(rule missing quotes?)" ), call.=FALSE )
             }
           }
           # execute f
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
     
     
     envS$s.not<-function(f){ #not before
       h<-function(input, exe=TRUE,  p=1){ #not before
         if(envS$DEVEL.DEBUG){
           cat("negate: input=",input," p=",p,"\n") ###good for debugging      
         }    
         exe<-FALSE
         # grab f
         if("peg.name" %in% class(f)){
           if(exists(f,envir=pegE)){ 
             f<-get(f,envir=pegE)
           } else {
             stop(paste("missing symbol:", f , "(rule missing quotes?)" ), call.=FALSE )
           }
         }
         # execute f      
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
         # grab f
         if("peg.name" %in% class(f)){
           if(exists(f,envir=pegE)){ 
             f<-get(f,envir=pegE)
           } else {
             stop(paste("missing symbol:", f , "(rule missing quotes?)" ), call.=FALSE )
           }
         }
         # execute f      
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
     
     envS$opt.0x<-function(f){ # f* :zero more repititions
       h<-function(input, exe=TRUE,  p=1){
         ok<-TRUE
         m<-0
         val<-c()
         if(pegE$.DEBUG.NODE==T){ 
           d.node<-list()
         }
         while(ok){
           # grab f
           if("peg.name" %in% class(f)){
             if(exists(f,envir=pegE)){ 
               f<-get(f,envir=pegE)
             } else {
               stop(paste("missing symbol:", f , "(rule missing quotes?)" ), call.=FALSE )
             }
           }
           # execute f      
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
         # grab f
         if("peg.name" %in% class(f)){
           if(exists(f,envir=pegE)){ 
             f<-get(f,envir=pegE)
           } else {
             stop(paste("missing symbol:", f , "(rule missing quotes?)" ), call.=FALSE )
           }
         }
         # execute f      
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
         # grab f
         if("peg.name" %in% class(f)){
           if(exists(f,envir=pegE)){ 
             f<-get(f,envir=pegE)
           } else {
             stop(paste("missing symbol:", f , "(rule missing quotes?)" ), call.=FALSE )
           }
         }
         # execute f      
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
     
     
}

