\name{plot.PEGResult}
\alias{plot.PEGResult}
\title{Plots the parsing result as a tree.}
\usage{
\method{plot}{PEGResult}(res, shadow = TRUE, show = "names",
  bg = ifelse(match(par("bg"), "transparent", 0), "white", par("bg")),
  border = TRUE, xpad = 1, ypad = 2.5, cex = 1, adj = 0.5, ...)
}
\arguments{
  \item{res,}{a result obtained from parsing with
  record=TRUE}

  \item{shadow,}{when set to TRUE, renders a shadow for all
  nodes. The default is TRUE}

  \item{show,}{a vector consisting of any combination of
  the following: \itemize{ \item{"names"} \emph{(default)},
  { When spefified displays the name (rule id) of the rule
  at that node(} \item{"args"}, {When specified, displays
  the text consumed at that node} \item{"vals"}, {When
  specified, displays the vaule returned that node}
  \item{"all"}, {Displays all of the above} }}
}
\description{
Plots a tree representing the result of applying a rule to
a text input, when that result was obtained with the record
parameter set to TRUE
}
\note{
The default value for \emph{show} is "names".
}
\examples{
peg<-new.parser()
  add_rule(peg, "A<-'a'")
  add_rule(peg, "B<-'b'")
  add_rule(peg, "D<-'d'")
  add_rule(peg, "C<-'c'")
  add_rule(peg,"ROOT<-A B C D")
  apply_rule(peg,"ROOT","abcd", record=TRUE)->res
  plot(res)
  plot(res, show="args")
  plot(res, show="vals")
  plot(res, show=c("names","args"))
  plot(res, show="all")
}

