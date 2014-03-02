
#' Plots the parsing result as a tree.
#' 
#' Plots a tree representing the result of applying a rule to a text input, when that result was 
#' obtained with the record parameter set to TRUE (Either by \code{\link{set_record_status}} or by 
#' as a parameter option in \code{\link{apply_rule}})
#' 
#' @param res, a result obtained from parsing with record=TRUE
#' @param shadow, when set to TRUE, renders a shadow for all nodes. The default is TRUE
#' @param show, a vector consisting of any combination of the following:
#' \itemize{
#'  \item{"names"} \emph{(default)}, { When spefified displays the name (rule id) of the rule at that node(}
#'  \item{"args"}, {When specified, displays the text consumed at that node}
#'  \item{"vals"}, {When specified, displays the vaule returned that node}
#'  \item{"all"}, {Displays all of the above}
#' }
#' @note The default value for \emph{show}  is "names". 
#' @examples
#'   peg<-new.parser()
#'   peg<-add_rule(peg, "A<-'a'")
#'   peg<-add_rule(peg, "B<-'b'") 
#'   peg<-add_rule(peg, "D<-'d'")
#'   peg<-add_rule(peg, "C<-'c'")
#'   peg<-add_rule(peg,"ROOT<-A B C D")
#'   apply_rule(peg,"ROOT","abcd", record=TRUE)->res
#'   plot(res)
#'   plot(res, show="args")
#'   plot(res, show="vals")
#'   plot(res, show=c("names","args"))
#'   plot(res, show="all")
#' @export
plot.PEGResult<-function(res, shadow=TRUE, show="names", bg = ifelse(match(par("bg"), "transparent", 0), "white", par("bg")), border = TRUE, xpad = 1.0, ypad = 2.5, 
                         cex = 1,  adj = 0.5, ...){
  if( is.null(res$debugNode) ){
    stop("Tree option not set, rerun parse with option record=True", call. = FALSE)
  }
  if("rules" %in% show){
    show<-c("names", show)
  }
  show.opts<-c("all","names","args","vals", "rules")
  if(!(any(show %in% show.opts))){
    stop("plot error: value of show must contain one or more of 'rules', 'names', 'args', 'vals', 'all'", call. = FALSE)
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
  if(!is.null(arrows)){
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
  
}
