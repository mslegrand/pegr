

getRuleMatrix<-function(peg){
#prep work begin 

#container for undefined rules
missing<-data.frame(id=NA,cid=NA)[numeric(0), ] 

#temp parser to extract identifiers from rule source
#pegT<-NULL
pegT<-new.parser()
pegT +
  c("Q1<- \"'\"", "#literal single quote")  +
  c("Q2<- '\"'" , "#literal double quote") +
  c("RNG<- '[' . '-' . ']' ", "{}", "#eat ranges") +
  c("ID<-( [a-z]/[A-Z] )  ( [a-z]/[A-Z]/[0-9] )* ", "{-}", "#Identifier")  +
  c("OTH<- !ID !Q1 !Q2 !RNG .", "{}", "#Any thing else")  +
  c("Q1p<- Q1 (!Q1 (Q1/ .))* Q1 ",  "{}", "#eat single quoted")  +
  c("Q2p<- Q2 (!Q2 (Q2/ .))* Q2 ",  "{}", "#eat double quoted")  +
  c("EXTRACT<- ( RNG / ID / OTH / Q1p / Q2p   )+ ", "#collect all identifers as list")

#get a vector of rules from the peg

# tst.data<-list(
#   Q1="Q1<- \"'\"",
#   Q2="Q2<- '\"'" ,
#   ID="ID<-( [a-z]/[A-Z] )  ( [a-z]/[A-Z]/[0-9] )* ",
#   OTH= "OTH<- !ID !Q1 !Q2 .",
#   Q1p="Q1p<- Q1 (!Q1 (Q1/ .))* Q1 ", 
#   Q2p="Q2p<- Q2 (!Q2 (Q2/ .))* Q2 ",  
#   EXTRACT="EXTRACT<- ( ID / OTH /  Q1p / Q2p )+ "
# )

#ids<-names(tst.data) #
ids<-pexGetIDs(peg)

ids<-sort(ids)

#create empty call matrix
mLen<-length(ids)
m<-matrix(0,mLen, mLen) 
rownames(m)<-ids
colnames(m)<-ids

#end of prep work

#loo through all definitions (where id is what is being defined)
for(id in ids){
  #src<-tst.data[[id]] #
  src<-pexGetRuleSource(peg, id) #get the source 
  apply_rule(pegT, "EXTRACT", src, exe=T)->res #try extract  ids from src
  if(status(res)==T){ #it worked!
    cids<-unlist(value(res))
    cids<-cids[-1] #get the extracted ids less the id on the left of the <-that is the def
    for(cid in cids){ 
     # browser()
      if( cid %in% ids ){ # is defined 
        m[cid,id]<-1
      } else {
        missing<-rbind(missing, data.frame (id=id, cid=cid) ) 
      }
    }  
  }
}

list(missing=missing, rule.matrix=m)
}
#1. get all ids and src
#2. for each rule.id get ids contained in src
#3. for each rule.id 
#    for each id in rule.source,  
#           check if rule id is defined, 
#                        if not record rule.id and id in missing df.
#                        else add to matrix.




get.rule.call.details<-function(peg){
  getRuleMatrix(peg)->res
  rule.matrix<-res$rule.matrix
  missing<-res$missing
  
  rule.matrix
  
  n<-dim(rule.matrix)[1]
  m.in<-diag(n)
  rownames(m.in)<-colnames(rule.matrix)
  colnames(m.in)<-colnames(rule.matrix)
  
  
  for(i in 1:n){
    m.out<-rule.matrix %*% m.in
    m.in<-m.in +m.out
  }
  return(list(m=m.out, missing=missing))
} 

get.rule.call.summary<-function(peg){
  details<-get.rule.call.details(peg)
  m.out<-details$m
  count.rules.rec<-diag(m.out)
  ids<-names(count.rules.rec)
  count.rules.calling<-apply(m.out, 1, sum) #gives number of rules calling this (index)
  roots<-ids[which(count.rules.calling==0)]
  count.rules.called<-apply(m.out, 2, sum) #gives number of rules this calls
  terminals<-ids[which(count.rules.called==0)]
  

  missing.rules<-details$missing
  rec.rules<-ids[which(count.rules.rec!=0)]
  root.rules<-ids[which(count.rules.calling==0)]
  terminal.rules<-ids[which(count.rules.called==0)]
  
  list(
    missing=missing.rules,
    recursives=rec.rules,
    roots=root.rules,
    terminals=terminal.rules 
  )
}

