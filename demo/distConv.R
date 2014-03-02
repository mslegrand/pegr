peg<-new.parser()
# rule set to convert distances to meters
peg<-add_rule(peg, "NUM<-[0-9]+ ('.' [0-9]*)?)", act = "list(  num=as.numeric(paste(v,collapse=''))  )", des = "numbers")
peg<-add_rule(peg, "IN<- NUM ' '* 'in' ", act="list( num=0.0254*v$num)", des="inches; ins 2 meters")
peg<-add_rule(peg, "FT<- NUM ' '* 'ft' ", act="list( num=0.3048*v$num)", des="feet; ft 2 meters")
peg<-add_rule(peg, "CM<- NUM ' '* 'cm' ", act="list( num=.01*v$num)", des="centimeters; cm 2 meters")
peg<-add_rule(peg, "M<- NUM ' '* 'm' ",  act="list(num=v$num)", des="meters; extract the num")
peg<-add_rule(peg, "SP<-(' ')", act="list()", des="spaces; eats spaces")
peg<-add_rule(peg, "GP<- (IN / FT / CM / M)", des="added for group t")
peg<-add_rule(peg, "LEN<-(GP SP+ LEN) / GP ", act="list(sum(unlist(v)))",  des="length; sum all the units" )

#first test the components
apply_rule(peg, "NUM", "3", exe=T)
apply_rule(peg, "IN", "1.0 in", exe=T)
apply_rule(peg, "FT", "2 ft", exe=T)
apply_rule(peg, "CM", "100 cm", exe=T)
apply_rule(peg, "M", "1 m", exe=T)
#now test together
apply_rule(peg, "LEN", "3 ft 3.3701 in", exe=T)
apply_rule(peg, "LEN", "3 m 4.8 cm", exe=T)
apply_rule(peg, "LEN",  "10 ft",exe=T)

