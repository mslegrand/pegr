\name{[[<-.pegR}
\alias{[[<-.pegR}
\title{Reset an existing rule: definition, description, action}
\usage{
\method{[[}{pegR}(parser, rule.id) <- value
}
\arguments{
  \item{parser,}{the peg parser containing the rule to be
  modified}

  \item{rule.id,}{the identifier of the rule to be
  modified}

  \item{value,}{the modifications to be applied. Can be
  either a named vector or a named list (see below), a
  unnamed character vector(see below), or NULL, which case
  the rule is deleted.}
}
\value{
peg parser
}
\description{
Used to modify the rule \emph{definition} and/or
\emph{description} and/or \emph{action} of of an exiting
rule
}
\details{
This is a very flexiable way of resetting the action,
description and even the definition of an existing rule.
This encompasses the functionality of
\code{\link{set_definition}}, \code{\link{set_description}}
and \code{\link{set_action}} This rule accepts either
\emph{named} vectors/lists or \emph{unnamed} vectors. In
both cases, the vectors are character vectors and the list
components are either strings or a function representing an
action.
}
\section{Using Named Vector/Lists}{
  For \bold{named vectors/lists} the valid names are:
  \itemize{ \item \emph{rule}: The name \emph{rule} is used
  to specify a field containing a \emph{rule definition}
  (a.k.a. source). (like "A<-'a'") to be associated with
  the named rule. \item \emph{des}: The name \emph{des} is
  used to specify a field containing a \emph{description}
  or comment to be associated with the named rule \item
  \emph{act}: The name \emph{act} is used to specify a
  field containing an \emph{action} to be associated with
  the rule. The action can either be NULL, or an existing
  function (taking a single list argument and returing a
  list) or text which represents the body of such a
  function. When NULL, any existing action is deleted.}
}

\section{Using Unnamed Vectors}{
  For \bold{unnamed vectors}, each vector consists of a
  single string of the form: \itemize{ \item
  \code{"...<-..."}: Specifies a \emph{rule definition}
  (a.k.a. source). (like "A<-'a'") \item \code{"#..."}
  Specifies \emph{description} or comment to be associated
  with the rule (like "# my clever rule") \item
  \code{"{...} "} Specifies an inline \emph{action} to be
  associated with the rule. (like
  "{list(toupper(unlist(v)))}" ) \item \code{"{}=..."}
  Specifies an external \emph{action} to be associated with
  the rule. (like "{}=fn", where fn is an existing function
  of one variable that returns a list.) \item
  \code{"{}=NULL"} Specifies to delete any existing to be
  associated with the rule. }
}

\section{Inline Action Short Cuts}{
  As a convenience the following short cuts are provided:
  \itemize{ \item \code{"{}"}: Specifies the action
  \code{"{list()}"}, which essentially says drop (ignore)
  the values produce by this rule. \item \code{"{-}"}
  Specifies the action \code{"list(paste(v,collapse=''))"},
  which says paste all the values together. }
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

