
preProcessArg<-function(arg, rule.id=NULL){
  arg<-as.list(arg)
  #   tmp<-deparse(substitute(arg))
  #   print( tmp )
  if( is.null(names(arg) ) ){
    names(arg)<-rep("",length(arg))
  } 
  which("function"==sapply(arg, function(x){class(x)}))->indx
  names(arg)[indx]<-"act"
  arg.w.name<-arg[names(arg)!=""]
  arg.wo.name<-arg[names(arg)==""]
  #process each blank name
  for(txt in arg.wo.name){
    w.name<-txt.2.fldName(txt)
    if(is.null(w.name)){
      stop("Do not understand parameter", txt, call.=FALSE)     
    }
    #append
    arg.w.name<-c(arg.w.name, w.name)
  }
  #now go through arg.w.name and validate
  for(type in names(arg.w.name)){
    if(type=="rule"){
      src<-validate.src( arg.w.name[[type]], rule.id)
    }
    else if ( type=="act"){
      action<-validate.act(arg.w.name[[type]])
    }
  }
  arg.w.name
}

validate.src<-function(src, rule.id=NULL){
  str_match(src, "^(.+)\\s*<-\\s*(.*)$")->mat
  #match rule.id
  if(!is.null(rule.id)){
    if(!identical(rule.id, mat[2])){
      stop("Rule.id=",rule.id,"and rule.source",src,"do not match\n") #OR RETURN FALSE???????????
    }  
  }
  #check quotes of rule.source
  #src<-mat[3]
  pos<-badQuotePos(src)
  if(pos!=0){
    stop("rule.source", src, "contains unbalanced quotes\n") #OR RETURN FALSE???????????
  }
  return(src)
}

validate.act<-function(action){
  #must be NULL or character vector
  if((!is.null(action)) & (!("character" %in% class(action)))){
    stop("Bad action\n")
  }
  if("character" %in% class(action)){
    str_match(action, "^\\s*$")->mat
    if(!is.na(mat[1])){ #makes an easy shortcut for list()
      action<-"list()"
    }
    str_match(action, "^\\s*-\\s*$")->mat
    if(!is.na(mat[1])){ #makes an easy shortcut for list(paste(v,collapse=''))
      action<-"list(paste(v,collapse=''))"
    }
    str_match(action, "^\\s*NULL\\s*$")->mat
    if(!is.na(mat[1])){ #makes an easy shortcut for NULL
      action<-NULL
    }   
    ok<-check.action.syntax.ok(action)
    if(!ok){
      stop("cannot set action\n")
    }
  }   
  return(action)
}

#extract type fron unnamed txt field
txt.2.fldName<-function( txt, rule.id=NULL ){
  txt<-str_trim(txt)
  mat<-str_match(txt, "^#\\s*(.+)")
  if(!is.na(mat[1])){
    return(list(des=mat[[2]]))
  }
  #try to match action inline
  mat<-str_match(txt,"^\\{(.*)\\}$")
  if(!is.na(mat[1])){
    return(list(act=mat[[2]]))
  }
  #try to match action external
  mat<-str_match(txt,"^\\{\\}=\\s*(.+)$")
  if(!is.na(mat[1])){
    fname<-mat[[2]] #if fname is NULL
    if(fname=="NULL"){
      return(list(act=NULL))
    } else if(exists(fname)){
      f<-get(fname)
      return(list(act=f))
    } else {
      stop("External Function Not Found\n",txt)
    }
  }  
  #try to match rule source
  str_match(txt, "^(.+)\\s*<-\\s*(.*)$")->mat
  if(!is.na(mat[1])){
    return(list(rule=txt))
  }
  #try to match commnent
  str_match(txt, "^\\s*#\\s*(.*)$")->mat
  if(!is.na(mat[2])){
    return(list(des=txt[[2]]))
  }
  #give up
  return(list(unknown=txt))
}
