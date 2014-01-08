\docType{package}
\name{pegr}
\alias{pegr}
\alias{pegr-package}
\title{Parsing Made Bearable With Pegr}
\description{
Pegr provides tools to parse using the parsing expression
grammar (PEG) as defined in Bryan Fords seminal work
\href{http://www.brynosaurus.com/pub/lang/peg.pdf}{ParsingExpressionGrammars:
A Recognition-Based Syntactic Foundation}.
}
\details{
This implementation contains the following benefits
\enumerate{ \item Easy debugging of rules, since we can set
any node to be the root \item A tool (qp) to quickly parse
simple expressions on the fly, great for learning PEG or
confirming those constructs. \item Printing a tree of all
nodes visited during a parse, again helpful in debugging
the rule set. \item Plotting a tree of nodes visisted
during a parse, again helpful in debugging \item Providing
a mechanism to add comments to nodes, just as we commonly
add comments to code }
}
\references{
ParsingExpressionGrammars: A Recognition-Based Syntactic
Foundation -slides
\url{http://www.brynosaurus.com/pub/lang/peg-slides.pdf}
}

