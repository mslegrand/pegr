# BEGIN PEX INTERFACE
pexGetStack<-function(pegR){
  pegR$GET_STACK()
}

pexDeleteRule<-function(pegR, rule.id){
 pegR$DELETE_RULE(rule.id)  
}

#pex are wrappers around the pegR to be more S like
pexSetRule<-function(pegR, rule){
  pegR$SET_RULE(rule)
}
pexGetRuleSource<-function(pegR, rule.id){
  pegR$GET_RULE_SOURCE(rule.id)
}
pexSetDescription<-function(pegR, rule.id, description){
  pegR$SET_DESCRIPTION(rule.id, description)
}
pexGetDescription<-function(pegR, rule.id){
  pegR$GET_DESCRIPTION(rule.id)
}
pexSetAction<-function(pegR, rule.id, action){
  pegR$SET_ACTION(rule.id, action)
}
pexGetAction<-function(pegR, rule.id){
  pegR$GET_ACTION(rule.id)
}
pexSetActionInfo<-function(pegR, rule.id, actionInfo){
  pegR$SET_ACTION_INFO(rule.id, actionInfo)
}
pexGetActionInfo<-function(pegR, rule.id){
  pegR$GET_ACTION_INFO(rule.id)
}
pexGetIDs<-function(pegR){
  pegR$GET_IDS()
}
pexSetStopLevel<-function(pegR, stop.level.limit){
  pegR$SET_STOP_LEVEL(stop.level.limit)
}
pexUnSetStopLevel<-function(pegR){
  pegR$UNSET_STOP_LEVEL()
}

pexIsDebugging<-function(pegR){
  pegR$GET_DEBUG_ON()
}

pexApplyRule<-function(pegR, rule.id, input.text, exe=NULL, record=NULL){  
  res<-NULL
  # two cases"
  if(pegR$pegE$.DEBUG_ON==TRUE){ 
    # case 1: if debugging
    pegR$pegE$.DEBUG$command.summary()
    more<-TRUE
    while(more){
      more <-FALSE
      withRestarts(
        pegR$APPLY_RULE(rule.id, input.text, exe, record)->res,
        quitDebug=function(){cat("rdb>  Rule debugger quiting!\n")},
        restartDebug=function(){cat("Restarting:\n"); more<<-TRUE}
      ) 
    } 
    cat(" Bye\n")
  } else { 
    #else case2: not debugging
    pegR$APPLY_RULE(rule.id, input.text, exe, record)->res    
  }
  res
}

ruleStruct<-function(name, def, descript=NULL, action=NULL){
  rs<-list(name=name, def=def, com=descript, act=action )
  class(rs)<-"ruleStructure"
  rs
}

pexSetDebugOn<-function(pegR, mode){
  pegR$SET_DEBUG_ON(mode)
}

pexGetRuleStructure<-function(pegR, rule.id){
  rs<-ruleStruct(
    rule.id, 
    pegR$GET_RULE_SOURCE(rule.id), 
    pegR$GET_DESCRIPTION(rule.id), 
    pegR$GET_ACTION_INFO(rule.id)
  )
  rs
}

pexSetRecordDefault<-function(pegR, on){
  pegR$SET_RECORD_DEFAULT(on)
}

pexGetRulesAsDataFrame<-function(pegR, ...){
  pegR$GET_DATA_FRAME(...)
}


#END PEX INTERFACE
