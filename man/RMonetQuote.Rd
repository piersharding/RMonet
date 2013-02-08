\name{RMonetQuote}
\alias{RMonetQuote}
\title{Quote escape a string ready for SQL input}
\description{
  Quote escape a string ready for SQL input
}
\usage{
RMonetQuote(str)
}%"
\arguments{
  \item{str}{String requiring SQL escaping}
}
\details{

  res <- RMonetQuote("\'some&string-that%requires =looking at")
  print(res)

}

\note{
  Not much to note here.
}

\value{
  Returns string
}
\examples{\dontrun{
# Close the connection
RMonetQuote("\'some&string-that%requires =looking at")
}}

\author{
  Piers Harding
}
\seealso{
  \code{\link{RMonet}}, \code{\link{RMonetConnect}}, \code{\link{RMonetGetInfo}}, \code{\link{RMonetQuery}}, \code{\link{RMonetLoadDataFrame}}, \code{\link{RMonetExecute}}, \code{\link{RMonetClose}}, \code{\link{RMonetStartTransaction}}, \code{\link{RMonetCommit}}, \code{\link{RMonetIsConnected}}, \code{\link{RMonetQuote}}, \code{\link{RMonetUnQuote}}, \code{\link{RMonetExists}}, \code{\link{RMonetListTables}}, \code{\link{RMonetListFields}}
}
\keyword{IO}
\keyword{SQL}
\keyword{MonetDB}
