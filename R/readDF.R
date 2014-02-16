
# just did syntax check for set action.
# do we need to do syntax check for add and <- ???
# 
# df1<-data.frame(rule.id=rule.id, rule.definition=rule.definition, 
#                 rule.description=rule.description, action.type=action.type, 
#                 action.specification=action.specification, stringsAsFactors=FALSE, ...)
# df<-rbind(df,df1)


#quick kludge for checking for matching quotes
badQuotePos<-function(spec){
  bqp<-function(s){
    state<-0
    pos<-0
    for(i in 1:nchar(s)){
      if(substr(s,i,i)=='"' ){
        if(!(state==-1)){ #not in ' '
          state<-1-state #flip bits
          pos<-i
        }
      } else if(substr(s,i,i)=="'"){
        if(!(state==1)){
          state<--(1+state)
          pos<-i
        }
      }
    }
    return(pos*abs(state)) # pos if state=!0    
  }
  rtv<-sapply(spec, bqp)
  rtv
}

check.action.syntax.ok<-function(action){
  tryCatch( parse(text=action), error=function(e) cat("Bad action syntax", action, "\n") )->x
  ifelse(is.null(x), FALSE, TRUE)
}


# isIDNameGood<-function(name){
#   #begins with cap?
#   #contains only letters, digits or underscore.
#   grepl("^[A-Z][A-Z,a-z,0-9,_]*$",name)
# }
# 
# isDataFrameGood<-function(df){ 
#   # check rule.id
#     mssg<-"Dataframe Column:"
#     # assert exist
#     if(! "rule.id" %in% names(df)){
#       return(paste(mssg,"Missing") ) 
#     }
#     # characters?
#     if(!"character" %in%  class(df$rule.id)){
#       return(paste(mssg,"Not character!") )
#     }
#       # no NA
#     if(any(is.na(df$rule.id))){
#       return(paste(mssg,"contains missing vlaue") )
#     }
#     # conform to namin conventions?
#     if(any(!isIDNameGood(df$rule.id))){
#       return(paste(mssg,"contains id with bad syntax"))
#     }
#     # no duplicates?
#     if(anyDuplicated(df$rule.id)>0){
#       return(paste(mssg,"contains duplicated ids"))
#     }
#   # check rul.source
#     mssg<-"Dataframe Column: rule.definition"
#     if(! "rule.definition" %in% names(df)){
#       return(paste(mssg,"Missing") ) 
#     }
#     # characters?
#     if(!"character" %in%  class(df$rule.definition)){
#       return(paste(mssg,"Not character!") )
#     }
#     # no NA
#     if(any(is.na(df$rule.definition))){
#       return(paste(mssg,"contains missing value") )
#     }    
#     # balanced quotes?
#     pos<-badQuotePos(df$rule.definition)
#     if(any(pos>0)){
#       return(paste(mssg,"contains unbalanced quotes") )
#     }
#     # assignment with left side matching rule.id
#     str_match(df$rule.definition, "^\\s*([A-Z][A-Z,a-z,0-9,_]*)\\s*<-" )->m1
#     str_match(df$rule.id, "^\\s*([A-Z][A-Z,a-z,0-9,_]*)\\s*")->m2
#     if(any(is.na(m1)) | any(is.na(m2)) ){
#       return(paste(mssg, "failed to extract rule from rule definition"))
#     } else {
#       mm1<-m1[,2]
#       mm2<-m2[,2]
#       if(any( mm1!=mm2 )){
#         return(paste(mssg, "rule in definition does not agree with rule.id"))
#       }
# 
#     } 
#   # check rule.description (optional)
#     mssg<-"Dataframe Column: rule.description"
#     if("rule.description" %in% names(df)){
#       if(! "character" %in% class(df$rule.description)){
#         return(paste(mssg,"Not character") ) # characters or NA
#       }      
#     } 
#   # check actions
#     # both action.type and action.specification exist or neither exist
#     if("action.type" %in% names(df) & !("action.specification" %in% names(df))){
#       return("Data has action.type but no action.specification")
#     }
#     if("action.specification" %in% names(df) & !("action.type" %in% names(df))){
#       return("Data has action.specification but no action.type")
#     }
#     if("action.type" %in% names(df) & ("action.specification" %in% names(df))){
#       # indices where defined must match
#       if(any(is.na(df$action.type)!=is.na(df$action.specification))){
#         return("NA's for action.type does not match NA's for action.specification")
#       }
#       #now run through the rows and check if the action is ok
#       errors<-c()
#       for( i in  1:nrow(df)){
#         type<-df$action.type[i]
#         spec<-df$action.specification[i]
#         if(type=="Inline"){
#           #check syntax of inline df$action.specification
#           ok<-check.action.syntax.ok(spec)
#           if(!ok){
#             paste("at row:",i,"action.specification: bad syntax")->err
#             errors<-c(errors, err)
#           } #end inline      
#         } else { #should only be external here
#           if (type=="External"){
#             # check for existence of fn in global.envir
#             ok<-exists(spec, envir=.GlobalEnv)
#             if(!ok){
#               paste("at row:",i,"action.specification: missing external")->err
#               errors<-c(errors, err)
#             } 
#           } #end external
#         }
#       }
#       if(length(errors)>0){
#         return(paste(errors, collapse="\n"))
#       }
#     }
#   TRUE
# }

include.readDF<-function(env=parent.frame() ){  
# 
#df is data frame
#returns TRUE upon success, false otherwise
env$add_data.frame<-function(genE, df){
# check data.frame 
  mssg<-"Dataframe Column "
  #1. must have both rule.id, rule.definition
  if(! "rule.id" %in% names(df)){   
    cat(mssg,"rule.id:", "Missing\n")
    return(FALSE)
  }
  if(! "rule.definition" %in% names(df)){   
    cat(mssg,"rule.definition:","Missing\n")
    return(FALSE)
  }
  #2. cannot have any na for rule.id and source.id
  # no NA
  if(any(is.na(df$rule.id))){
    cat(mssg,"rule.id:", "Contains NA\n")
    return(FALSE)
  }
  if(any(is.na(df$rule.definition))){
    cat(mssg,"rule.definition:", "Contains NA\n")
    return(FALSE)
  }
  #3. must have both or neither action  
#   if("action.type" %in% names(df) & ! "action.specification" %in% names(df) ){
#     cat(mssg,"action.type exists but no actions.specification\n")
#     return(FALSE)
#   }
#   if("action.specification" %in% names(df) & ! "action.type" %in% names(df) ){
#     cat(mssg,"action.specification exists but no action.type\n")
#     return(FALSE)
#   }
  #4. rule.id, rule.definition, action.specification should be character
  #df <- rapply(df, as.character, classes="factor", how="replace")
  if(is.factor(df$rule.id)){
    df$rule.id<-as.character(df$rule.id)
  }
  if(is.factor(df$rule.definition)){
    df$rule.definition<-as.character(df$rule.definition)
  }
  if(is.factor(df$rule.description)){
    df$rule.description<-as.character(df$rule.description)
  }  
  if("action.specification" %in% names(df) & is.factor(df$action.specification)){
    df$action.specification<-as.character(df$action.specification)
  }
#   if("action.type" %in% names(df) & is.factor(df$action.type)){
#     df$action.type<-as.character(df$action.type)
#   }
  if(!"character" %in% class(df$rule.id)){
    cat(mssg,"rule.id has wrong class\n")
    return(FALSE)    
  }
  if(!"character" %in% class(df$rule.definition)){
    cat(mssg,"rule.definition has wrong class\n")
    return(FALSE)    
  }
  if(!"character" %in% class(df$rule.description) & !("logical" %in% class(df$rule.description))){
    cat(mssg,"rule.description has wrong class\n")
    return(FALSE)    
  }
#   if(!"character" %in% class(df$action.type)){
#     cat(mssg,"action.type has wrong class\n")
#     return(FALSE)    
#   }
  if(!"character" %in% class(df$action.specification) & !("logical" %in% class(df$action.specification))){
    cat(mssg,"action.specification has wrong class\n")
    return(FALSE)    
  }

  process.DF2Escape<-function(txt){
    txt<-str_replace_all(txt, "\\\\n", "\n")
    txt<-str_replace_all(txt, "\\\\t", "\t")
#    txt<-str_replace_all(txt, "\\\\s", "\s")
 #   txt<-str_replace_all(txt, "\\\\", "\\")
    txt
  }

  #next process the rows one at a time
  for(i in 1:nrow(df) ){
    # we already checked rule.id matches rule.definition
    # and action is ok
    rule.id<-str_trim(df$rule.id[i]) #need to trim?
    ruleDef<-df$rule.definition[i]
    ruleDef<-n<-process.DF2Escape(ruleDef)
    res<-genE$DEFINITION(ruleDef)
    if(res$ok==TRUE){
      #record source
      genE$pegE$.SOURCE.RULES[[rule.id]]<-ruleDef
      #description:
      if("rule.description" %in% names(df)){
        description<-df$rule.description[i]
        if(! is.na(description) ){
          description<-process.DF2Escape(description)
          genE$pegE$.RULE_DESCRIPT[[rule.id]]<-description
        }
      }
      #  action:
      if("action.specification" %in% names(df)){
        #browser()
        spec<-df$action.specification[i]
        if( is.na(spec) ){
          next
        } else {
          ok<-check.action.syntax.ok(spec)
          if(ok==TRUE){
            actionFn<-paste("function(v){",spec,"}")
            #pegR$genE$pegE$.ACTION[[rule.id]]<-eval(parse(text=action))  
            action<-eval(parse(text=actionFn))             
            genE$pegE$.ACTION[[rule.id]]<-action 
            #pegR$genE$pegE$.ACTION_NAMES[[rule.id]]<-c("Inline",action)
            #actionInfo<-c("Inline",spec) 
            genE$pegE$.ACTION_INFO[[rule.id]]<-spec #actionInfo
          } else { #bail
            cat("Error at row :",i, " Action syntax invalid", "\n" )
            return(FALSE) 
          }          
          
        }
      }      
    } else { # res$ok==FALSE
      cat("Error at row :",i, " Invalid Rule Syntax", "\n" )
      return(FALSE)      
    }
  } #end of the row iterations
  return(TRUE ) 
}

} #end of include

# peg<-new.parser()
# genE<-peg$getGenE()
# 
# fn<-function(x){list()}
# as<-c("list()", "fn")
# at<-c("Inline","External")
# rd<-c("aaa",NA)
# rs<-c("A<-'a'", "B<-'b'")
# ri<-c('A','B')
# df<-data.frame(rule.definition=rs, rule.id=ri, 
#                rule.description=rd,  action.type=at, 
#               action.specification=as, stringsAsFactors=FALSE)
# isDataFrameGood(df)
