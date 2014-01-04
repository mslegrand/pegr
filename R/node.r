library(RJSONIO)

new.node<-function(id,name,data=list(), children=list() ){
#   lapply(children, function(x){length(x)>0})->indx
#   children<-children[indx==T] #remove all empty lists
  n<-list(id=id,name=name,data=data,children=children)
  class(n)<-c("node",class(n))
  n
}

addChild<-function(parent, child){
	parent$children<-c(parent$children, list(child))
}




to.JSON<-function(n){ #uses input txt, called arg
  id<-paste("\"id\":\"", n$id,"\"",sep="")
  name<-paste("\"name\":\"", n$name, "\"", sep="")
  if(length(names(n$data))==0)
    data<-"{\"tip\":\"bye\"}"
  else{
    #dd<-paste(n$data,collapse="<br>") #temp kludge
    consumes<-paste( 
      n$data$consumes$t1, 
      "<span style='color: #32CD32;'>", n$data$consumes$t2, "</span>",
      n$data$consumes$t3, 
      sep="")
    print(n$data$consumes)
    #dd<-paste("consumes: ", n$data$consumes, "<br>","value: ",n$data$value,sep="")
    dd<-paste("consumes: ", consumes, "<br>","value: ",n$data$value,sep="")
    data<-paste("{\"tip\":\"",dd,"\"}",sep="")
  }
  #data<-"{\"tip\":\"hello\"}"
  data<-paste("\"data\":", data)
  children<- paste(lapply(n$children, to.JSON),collapse=",\n")
  children<-paste("\n\"children\": [",children,"\n]")
  out<-paste(id, name, data, children, sep=", ")
  out<-paste("{",out,"}")
  out
}

make.rand.Tree<-function(parentID, kidMin, kidMax){
  paste(letters[sample(26,3)],collapse="")->name
  id<-paste(parentID,name, sep="-")
  kidCnt<-sample(kidMin:kidMax,1)
  if(kidCnt>0){
    kidMin=max(0, min(kidMin-1, kidCnt -1))
    kidMax=max(0, min(kidCnt,kidMax-1))
    kids<-lapply(1:kidCnt, function(x) make.rand.Tree(id,kidMin, kidMax))
  } else {
    kids<-list()
  }
  tree<-new.node(id, name, data=list(), children=kids)
}

#  tree<-make.rand.Tree("ID",2,5)
#  cat(to.JSON(tree))