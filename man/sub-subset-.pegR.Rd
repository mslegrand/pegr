\name{[[<-.pegR}
\alias{[[<-.pegR}
\title{Reset an existing rule}
\usage{
\method{[[}{pegR}(parser, rule.id) <- value
}
\arguments{
  \item{parser,}{the peg parser containing the rule to be
  modified}

  \item{rule.id,}{the identifier of the rule to be
  modified}

  \item{value,}{the modifications to be applied. Can be
  either a named vector or a named list, a unnamed
  character vector, or NULL, which case the rule is
  deleted. For named vectors/lists the names are: \itemize{
  \item {rule} The source of the rule (like "A<-'a'") \item
  {des} A comment to be associated with the rule \item
  {act} An action to be associated with the rule }}
}
\value{
peg parser
}
\description{
Used to Modify the rule definition and or description or
value of of an exiting rule
}
\examples{
# Modfiying a rule (adding a comment and inline action):
peg<-new.parser()
peg + "A<-'a'"
peg
peg[["A"]]<-"A<-'ab'"
peg
peg[["A"]]<-c("A<-'xx'", des="replace xx by a", act="list('a')")

# Another way of doing the same thing (adding a comment and inline action):
peg<-new.parser()
peg + "A<-'a'"
peg
peg[["A"]]<-"A<-'ab'"
peg
peg[["A"]]<-c("A<-'xx'", "#replace xx by a", "{list('a')}")

# External functions can be added either by,
fn<-function(v){paste(v,collapse="")}

# The following produce the same result:
peg[['A']]<-list(act="list()")
peg[['A']]<-"{}" # A shortcut

# Deleteing a rule
peg<-new.parser()
peg + "A<-'a'" + "B<-'b'"
peg[["A"]]<-NULL
peg
}

