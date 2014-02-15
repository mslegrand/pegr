\name{summary.pegR}
\alias{summary.pegR}
\title{Summarizes of the parser rule structure}
\usage{
\method{summary}{pegR}(pegR)
}
\description{
Summary provides the following : \enumerate{ \item
\emph{Undefined Rules} This lists calls to rules that have
not yet been defined. \item \emph{Recursives} This list
rules which may directly (or indirectly) call themselves
\item \emph{Roots} This lists rules which are not called by
any rule and may only play the role of a root. \item
\emph{Terminals} This lists rules which call do not call
any rule and may only play the role of a leaf (terminal) }
}

