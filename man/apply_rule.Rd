\name{apply_rule}
\alias{apply_rule}
\title{Applies the rule to the specified text}
\usage{
apply_rule(parser, rule.id, input.text, exe = FALSE, debugTree = FALSE)
}
\arguments{
  \item{parser,}{a peg parser produced by new.parser}

  \item{rule.id,}{a character string naming the rule}

  \item{arg,}{a character string to be parsed}

  \item{exe,}{(optional, default=FALSE) a flag indicate
  whether actions should be performed. when FALSE, no
  actions will be executed}

  \item{debugTree,}{(optional, default=FALSE) a flag which
  when set produces tree of snapshots of all nodes visited
  during a successful parse.}
}
\value{
A PEGResult, a container containing the status of the
parsing attempt, a record of the characters parsed, and a
list of one or more values computed during the parse. If
exe not set (FALSE) or no actions have been attached to the
rules, then the values will simply be a list of all
literals (characters, or strings) that appeared in the
input text.
}
\description{
That is invoke the parser to parse a given input text using
the specified rule as the root.
}
\examples{
# The simplest example: a parser that only accepts a single character 'a'
peg<-new.parser()
add_rule(peg, "A<-'a'")
res<-apply_rule(peg, 'A', "a")
res # the results

# A more complex example:
# A number parser: Extracts number
peg<-new.parser()
add_rule(peg, "NUM<-[0-9]+ ('.' [0-9]*)?)", act = "paste1(v)", des = "numbers")
apply_rule(peg, "NUM", "12.3", exe=T)
peg<-new.parser()
# rule set to convert distances to meters
add_rule(peg, "NUM<-[0-9]+ ('.' [0-9]*)?)", act = "list(  num=as.numeric(paste(v,collapse=''))  )", des = "numbers")
add_rule(peg, "IN<- NUM ' '* 'in' ", act="list( num=0.0254*v$num)", des="inches; ins 2 meters")
add_rule(peg, "FT<- NUM ' '* 'ft' ", act="list( num=0.3048*v$num)", des="feet; ft 2 meters")
add_rule(peg, "CM<- NUM ' '* 'cm' ", act="list( num=.01*v$num)", des="centimeters; cm 2 meters")
add_rule(peg, "M<- NUM ' '* 'm' ",  act="list(num=v$num)", des="meters; extract the num")
add_rule(peg, "SP<-(' ')", act="list()", des="spaces; eats spaces")
add_rule(peg, "GP<- (IN / FT / CM / M)", des="added for group t")
add_rule(peg, "LEN<-(GP SP+ LEN) / GP ", act="list(sum(unlist(v)))",  des="length; sum all the units" )

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

}

