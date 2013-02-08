\name{RMonetClose}
\alias{RMonetClose}
\title{MonetDB Close database connections}
\description{
  Close an open connection to a MonetDB Database
}
\usage{
RMonetClose(con)
}%"
\arguments{
  \item{con}{an Open MonetDB Connection handle}
}
\details{
  \code{RMonetClose} closes aspecific connection to a MonetDB database
}

\note{
  Not much to note here.
}

\value{
  Returns true or false
}

\author{
  Piers Harding
}
\seealso{
  \code{\link{RMonet}}, \code{\link{RMonetConnect}}, \code{\link{RMonetGetInfo}}, \code{\link{RMonetQuery}}, \code{\link{RMonetLoadDataFrame}}, \code{\link{RMonetExecute}}, \code{\link{RMonetClose}}, \code{\link{RMonetStartTransaction}}, \code{\link{RMonetCommit}}, \code{\link{RMonetIsConnected}}, \code{\link{RMonetQuote}}, \code{\link{RMonetUnQuote}}, \code{\link{RMonetExists}}, \code{\link{RMonetListTables}}, \code{\link{RMonetListFields}}
}
\keyword{IO}
\keyword{SQL}
\keyword{MonetDB}
