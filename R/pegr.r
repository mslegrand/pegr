#MAIN PEG GENERATOR
#' Parsing Made Bearable With Pegr
#' 
#' Pegr provides tools to parse using the parsing expression grammar (PEG) as defined
#' in Bryan Fords seminal work \href{http://www.brynosaurus.com/pub/lang/peg.pdf}{ParsingExpressionGrammars: A Recognition-Based Syntactic Foundation}. 
#' 
#' This implementation contains the following benefits
#' \enumerate{
#' \item Easy debugging of rules, since we can set any node to be the root
#' \item Printing a tree of all nodes visited during a parse, again helpful in debugging the rule set.
#' \item Plotting a tree of nodes visisted during a parse, again helpful in debugging
#' \item Providing a mechanism to add comments to nodes, just as we commonly add comments to code
#' }
#' 
#' @references ParsingExpressionGrammars: A Recognition-Based Syntactic Foundation -slides
#' \url{http://www.brynosaurus.com/pub/lang/peg-slides.pdf}
#' @import memoise
#' @docType package
#' @name pegr
NULL


#' @title add_rule
#' Adds a rule to the parser
#' 
#' @param parser, a peg parser produced by  new.parser
#' @param rule, a quoted string that defines a rule according to the PEG Grammer
#' @return Status and the rule processed
#' @export
add_rule<-function(parser, rule){
  if( !( "genE" %in% class(parser) ) ){ stop("first argument not a parser") }  
  res<-parser$DEFINITION(rule) 
  if(res$ok==TRUE){
    name<-strsplit(rule,"<-")[[1]][1]
    parser$pegE$.SOURCE.RULES[[name]]<-rule    
  } else {
    stop(paste("invalid syntax:",rule))
  }
  invisible( list(ok=res$ok, parsed=substr(rule,1,res$pos) ) )
}

#' Attach an action to the rule specified by rule.id
#' 
#' @param parser, a peg parser produced by  new.parser
#' @param rule.id, a character string naming the rule
#' @param action to be attached to the specified rule. The action may be
#' either a function acceptiong a list as input and a list as output or
#' a string of text which may be intrepted as a function body that returns 
#' a list
#' @export
set_action<-function(genE, rule.id, action){
  #TODO:  ( expression?)
  #TODO: refactor using switch?
  if(!("genE" %in% class(genE))){ stop("first argument not a peg parser")}  
  if( rule.id %in% rule_ids(genE)){
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

#' Attaches an (optional) description to the given rule.
#' 
#' A description
#' should be used to comment a given rule
#' 
#' @param parser, a peg parser produced by  new.parser
#' @param rule.id, a character string naming the rule
#' @param description, a text string describing the rule
#' @export
set_description<-function(genE, rule.id, description){
  if(!("genE" %in% class(genE))){ stop("first argument not a peg parser")}  
  if( rule.id %in% rule_ids(genE)){
    genE$pegE$.RULE_DESCRIPT[[rule.id]]<-description
    invisible(TRUE)
  } else {
    stop("cannot add description: invalid rule identifier")
  }
}

#' Gets a description of a given rule
#' 
#' @param rule.id, a character string naming the rule
#' @param parser, a peg parser produced by  new.parser
#' @return description, a character string describing the parser
#' @export
get_description<-function(genE, rule.id){
  if(!("genE" %in% class(genE))){ stop("first argument not a peg parser")}  
  if( rule.id %in% rule_ids(genE)){
    description<-genE$pegE$.RULE_DESCRIPT[[rule.id]]
    return(description)
  } else {
    stop("cannot add description: invalid rule identifier")
  }
  
}

#' Deletes the given rule form the parser.
#' 
#' @param parser, a peg parser produced by  new.parser
#' @param rule.id, a character string naming the rule
#' @export
delete_rule<-function(genE, rule.id){
  #delete rule 
  if(!("genE" %in% class(genE))){ stop("first argument not a peg parser")}  
  genE$pegE$.SOURCE.RULES[[rule.id]]<-NULL
  genE$pegE$.ACTION[[rule.id]]<-NULL
  genE$pegE$.RULE_DESCRIPT[[rule.id]]<-NULL
  rm(list=rule.id, envir=genE$pegE)    
}

#' Lists all Rules contained in the parser
#' @param parser, a peg parser produced by  new.parser
#' @export
rule_ids<-function(genE){
  if(!("genE" %in% class(genE))){ stop("argument not a peg parser")}
  ls(envir=genE$pegE)->tmp
  if( any(grepl("atom.",tmp) ) ){
    tmp<-tmp[-grep("atom.",tmp)]    
  }
  tmp
}

#' Gets the inner environment of the parser
#' @export
get_pegE<-function(genE){
  if(!("genE" %in% class(genE))){ stop("first argument not a generator")}  
  genE$pegE
}

#' Invoke the parserto parse using the rule.id as the root.
#' 
#' @param parser, a peg parser produced by  new.parser
#' @param rule.id, a character string naming the rule
#' @param arg, a character string to be parsed
#' @param exe, a flag indicate whether actions should be performed. 
#' when false, no actions will be executed
#' @param debugTree, a flag which when set produces tree of snapshots of all nodes
#' visited during the parsing process
#' @export
apply_rule<-function(parser, rule.id, arg, exe=FALSE, debugTree=FALSE){
  if(!("genE" %in% class(parser))){ stop("first argument not a peg parser")}  
  if( !( rule.id %in% rule_ids(parser) ) ){stop("cannot parse: invalid rule identifier")}
  parser$pegE$.DEBUG.NODE<-debugTree
  parser$pegE[[rule.id]](arg, exe)->res
  if(!"list" %in% (class(res)) ){ stop("Bad Action Rule: resulting value is not a list")}
  res$Call<-list(rule.id=rule.id, arg=arg)
  res$options<-list(exe=exe, debugTree=debugTree)
  class(res)<-c("PEGResult")
  res
}

#' Prints the parser container
#' @export
print.pegE<-function(pegE){
  #list the rules in this peg
  ls(envir=pegE)
}


#' Summarizes a parsing result
#' @export
summary.PEGResult<-function(res){
  if(!("PEGResult" %in% class(res))){ stop("Argument not a peg parsing result")}
  s<-list(
  Call=paste0(res$Call$rule.id, "(" ,res$Call$arg, ")" ),
  Options=paste("Options:", "Apply Actions=",res$options$exe,"Make Tree=",res$options$debugTree,""),
  Status<-paste("Status:", ifelse(res$ok,"Success","Failure"),"")
  )
  if(res$ok){
    s$Processed<-paste("Processed:", res$pos, "out of", length(res$arg), "")
    val<-paste(res$val,collapse=",")
    s$val<-paste("Evaluates to: list(", val, ")\n")
  }
  class(s)<-"summary.PEGResult"
}

print.summary.PEGResult<-function(sum){
  cat(paste(sum,collapse="\n"))
}

#' prints the value of the parsing result
#' @export
print.PEGResult<-function(res){
  if(!("PEGResult" %in% class(res))){ stop("Argument not a peg parsing result")} 
  cat(paste0("Call:", paste0(res$Call$rule.id, "(",res$Call$arg,")\n")  ))
  cat(paste("Options:", "Apply Actions=",res$options$exe,"Make Tree=",res$options$debugTree,"\n"))
  cat(paste("Status:", res$ok, "\n") ) 
  cat(paste("Consumed: (", res$pos , "/", nchar(res$Call$arg) ,")\n" ))
  if(res$ok==TRUE){
    val<-paste(res$val,collapse=",")
    cat(paste("Evaluates to: list(", val, ")\n"))
  } else {
    cat("Status: Failure\n")
  }
  invisible(res)
}


#' Returns the Status of a parsing  result
#' 
#' @param A result from parsing
#' @return TRUE if successful, FALSE otherwise
#' @export
#' @examples 
#' parser<-new.parser()
#' #rule to test for string of a's followed by an equal number of b's
#' add_rule(parser, "S<-'a' S 'b'") 
#' res<-apply_rule(parser, 'S', 'aaaabbbb')
#' status(res)
#' res<-apply_rule(parser, 'S', 'aaabbbb')
#' status(res)
status<-function(res){
  if(!("PEGResult" %in% class(res))){ stop("Argument not a peg parsing result")} 
  res$ok
}

#' Returns the value of a parsing  result
#' 
#' @return A list containing the values computed
#' @export
value<-function(res){
  if(!("PEGResult" %in% class(res))){ stop("Argument not a peg parsing result")} 
  return(res$val) 
}

#' Returns the subset of the input text consumed during the parse
#' @export
consumed<-function(res){
  if(!("PEGResult" %in% class(res))){ stop("Argument not a peg parsing result")} 
  return(substring(res$Call$arg,1,res$pos))  
}

#' Prints the result of the parsing tree. 
#' @export
tree<-function(res){
  if( is.null(res$debugNode) ){
    stop("Tree option not set, rerun parse with option debugTree=True")
  }
  #requires 2 passes, 1 for the links/levels, second actualy prints
  node.print<-function(n, indent="", lastChild=TRUE){
    #s<-paste(rep("",indent),collapse=" ")
    si<-paste0(indent,"__")
    arg<-n$data$consumes$t2 #paste(n$data$consumes, collapse="")
    s<-paste0(si,"__", n$data$name, "(",arg , ") = list(", n$data$value ," )","\n") 
    cat(s)
    if(lastChild==TRUE){
      substr(indent,nchar(indent),nchar(indent))<-" "
    }
    #indent2<-paste(indent,"    ")
    indent1<-paste(indent,"   |")
    indx=1
    if(length(n$children)>0){
      for(child in n$children){
        if(indx<length(n$children)){
          node.print(child, indent1, FALSE)
          indx<-indx+1
        } else {
          node.print(child, indent1, TRUE)
          indx<-1
        }     
      }
    }
  }
  node.print(res$debugNode[[1]]) #!!!!
}


# plot.PEGResult<-function(res, bg = ifelse(match(par("bg"), "transparent", 0), "white", par("bg")), border = TRUE, xpad = 1.0, ypad = 2.5, 
#                          cex = 1, shadow=TRUE, adj = 0.5, ...){
#   if( is.null(res$debugNode) ){
#     stop("Tree option not set, rerun parse with option debugTree=True")
#   }
#   rootNode<-res$debugNode
#   #save old settings
#   cex=1
#   oldcex <- par("cex")
#   par(cex = cex)
#   adj<-.5
#   
#   if(!exists("main")){
#     main=paste("Parse Tree for node:", rootNode$data$name )
#   } 
#   
#   #set up graph screen
#   plot(0, main = main, xlim = c(0, 1), ylim = c(0, 1), xlab = "", ylab = "", type = "n", axes = FALSE)
#   #initialized nodes, arrows, boxWidths
#   nodes<-data.frame()
#   arrows<-c()
#   boxWidths<-c(0) #actually might be better to call level width
#   boxHeights<-0
#   xp<-strwidth("m") # a padding
#   
#   oval<-function(xl, yb, xr, yt,col = bg, border = border,...){
#     r<-(yt[1]-yb[1])/2
#     n<-20
#     theta<-seq(from=-pi/2,to=pi/2, length.out=n)
#     xcr<-r*cos(theta)
#     ycr<-r*sin(theta)
#     for(i in 1:length(xl)){
#       x1<-xcr+xr[i]-r
#       x2<--xcr+xl[i]+r
#       x<-c(x1,x2)
#       y1<-ycr+r+yb[i]
#       y2<--ycr+r+yb[i]
#       y<-c(y1,y2)
#       polygon(x,y, col=col, border=border)
#     }
#   }
#   
#   traverseOrd<-function(nn, level, pos, width){ #width is the full width 
#     nodes<<-rbind(nodes, data.frame(id=nn$id, pos=pos, level=level, text=nn$data$name))
#     boxWidths[level+1]<<-max(boxWidths[level+1], strwidth(nn$data$name)+xp) #update box width
#     boxHeights<<-max(boxHeights, strheight(nn$data$name)) #update box height
#     i<-0
#     n<-length(nn$children)
#     if(n==0){ return(NULL) }
#     if(n==1){ 
#       delta<-width/2
#       childWidth<-width
#     } else {
#       delta<-width/(n-1)
#       childWidth<-width/n #something slightly smaller than delta
#     }
#     childLevel<-level+1
#     boxWidths<<-c(boxWidths,0) #extend the boxWidths to accommadate the kids
#     i<-0
#     for(child in nn$children){
#       childPos<-delta*i - width/2 + pos
#       arrows<<-rbind(arrows, matrix(c(nn$id, child$id),1,2) ) #maybe we should record level position instead
#       traverseOrd(child, childLevel, childPos, childWidth)
#       i<-i+1
#     }
#   }
#   traverseOrd(rootNode, 0, 0, 2) #do the traveral
#   nodes$dbpos<-nodes$pos
#   nodes$pos<-sapply(nodes$pos, function(x){which(x==sort(unique(nodes$pos)))})
#   #now we plot the results
#   #compute the x for each box (recall level gives the x position)
#   #we compute the x-center of a node by x[level+1]
#   x<-boxWidths+3*xp
#   x<-cumsum(x)
#   xl<-x-(xp+boxWidths)-2*xp
#   xc<-xl+boxWidths/2
#   xr<-xl+boxWidths
#   nodes$xl<-xl[nodes$level+1]
#   nodes$xc<-xc[nodes$level+1]
#   nodes$xr<-xr[nodes$level+1] 
#   #we compute the y-center of a node by y[pos]
#   y<-nodes$pos
#   max(y)->noOfRows
#   delta<-1.0/noOfRows
#   nodes$yt<-delta*(nodes$pos) + boxHeights*.7
#   nodes$yc<-delta*(nodes$pos) 
#   nodes$yb<-delta*(nodes$pos) -  boxHeights*.7
#   #   
#   #rect(nodes$xl, nodes$yb, nodes$xr, nodes$yt,col = bg, border = border)
#   if(shadow==TRUE){
#     xpp<-xp/4
#     oval(nodes$xl+xpp, nodes$yb-xpp, nodes$xr+xpp, nodes$yt-xpp, col = "gray", border = FALSE)    
#   }
#   oval(nodes$xl, nodes$yb, nodes$xr, nodes$yt,col = bg, border = border)
#   
#   #   #compute the y for each box (recall pos gives the y position)
#   #plot the boxes
#   adj<-.5
#   args <- list(x = nodes$xc, y = nodes$yc, labels = nodes$text,  adj = adj, col = ifelse(colSums(col2rgb(bg) * c(1, 1.4, 0.6)) < 350, "white", "black"))
#   args <- modifyList(args, list(...))
#   do.call(text, args)
#   par(cex = oldcex)
#   
#   cubic<-function(p.x, p.y, q.x, q.y, n=20) {
#     print(paste(p.x,p.y,q.x,q.y))
#     f<-function(x){
#       (x^3)/3 -(p.x + q.x) * (x^2)/2 + q.x*p.x*x
#     }
#     K<-( q.y-p.y )/( f(q.x)-f(p.x) )
#     C<-p.y-K*f(p.x)
#     x<-seq(from=p.x, to=q.x, length=n)
#     y<-K*f(x)+C
#     df<-data.frame(x=x,y=y) 
#     lines(df$x, df$y)
#   }
#   
#   for(i in 1:nrow(arrows)){
#     id.from<-arrows[i,1]
#     id.to<-arrows[i,2]
#     r1<-subset(nodes, nodes$id==id.from)
#     r2<-subset(nodes, nodes$id==id.to)
#     p.x<-xr[r1$level+1]
#     q.x<-xl[r2$level+1]
#     p.y<-delta*r1$pos
#     q.y<-delta*r2$pos
#     cubic(p.x, p.y, q.x, q.y)
#   }
#   
# }

#' Plots the parsing result as a tree.
#' @export
plot.PEGResult<-function(res, show="names", bg = ifelse(match(par("bg"), "transparent", 0), "white", par("bg")), border = TRUE, xpad = 1.0, ypad = 2.5, 
                         cex = 1, shadow=TRUE, adj = 0.5, ...){
  if( is.null(res$debugNode) ){
    stop("Tree option not set, rerun parse with option debugTree=True")
  }
  if("rules" %in% show){
    show<-c("names", show)
  }
  show.opts<-c("all","names","args","vals", "rules")
  if(!(any(show %in% show.opts))){
    stop("plot error: value of show must contain one or more of 'rules', 'names', 'args', 'vals', 'all'")
  }
  boxHeight.scale.factor<-.7
  if( any(show.opts[show.opts!="names"] %in% show )){ #anything except names will grow
    boxHeight.scale.factor<-.9
  } 
  
  rootNode<-res$debugNode[[1]]
  #save old settings
  #cex=.5
  oldcex <- par("cex")
  par(cex = cex)
  adj<-.5
  
  if(!exists("main")){
    tmp<-c()
    if("all" %in% show| "names" %in% show){tmp<-c(tmp,"Rules")}
    if("all" %in% show| "args" %in% show){ tmp<-c(tmp,"Args")}
    if("all" %in% show| "vals" %in% show){ tmp<-c(tmp,"Vals")}
    tmp<-paste(tmp, collapse=", ")
    main=paste(tmp,"when Parsing:", rootNode$data$name )
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
    text<-""
    if("names" %in% show |"all" %in% show){
      text<-nn$data$name
    }      
    if("args" %in% show | "all" %in% show ){
      arg<-nn$data$consumes$t2 #paste(n$data$consumes, collapse="")
      text<-paste(text, "(",arg , ")")
    }
    if("vals" %in% show | "all" %in% show ){
      text<-paste(text,"= list(", nn$data$value ," )" )
      
    }
    
    nodes<<-rbind(nodes, data.frame(id=nn$id, pos=pos, level=level, text=text , kids=length(nn$children)))
    boxWidths[level+1]<<-max(boxWidths[level+1], strwidth(text)+xp) #update box width
    boxHeights<<-max(boxHeights, strheight(text)) #update box height
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
  #nodes$dbpos<-nodes$pos
  leaves<-subset(nodes, nodes$kids==0)
  leaves$pos<-sapply(leaves$pos, function(x){which(x==sort(unique(leaves$pos)))})
  traverseOrd2<-function(nn){
    if(length(nn$children)==0){
      pos<-leaves$pos[leaves$id==nn$id]
    } else {
      pos<-mean(sapply(nn$children, traverseOrd2) )
    }
    nodes$pos[nodes$id==nn$id]<<-pos
    pos   
  }
  traverseOrd2(rootNode)
  
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
  nodes$yt<-delta*(nodes$pos) + boxHeights*boxHeight.scale.factor
  nodes$yc<-delta*(nodes$pos) 
  nodes$yb<-delta*(nodes$pos) -  boxHeights*boxHeight.scale.factor
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
    #print(paste(p.x,p.y,q.x,q.y))
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

#todo:
#turn on all memoize
#print history of run 
#print peg(parse) history #history(pegE.result)
#print peg(parse) final state 
#print rule name/ description describe(pegE)
#print all rules: rules(pegE)
#add rule objects for printing, applying etc.
#summary(pegE)

