\name{RMonetUnQuote}
\alias{RMonetUnQuote}
\title{Unquote an SQL encoded string}
\description{
  Unquote an SQL encoded string
}
\usage{
RMonetUnQuote(str)
}%"
\arguments{
  \item{str}{string that has been quote escaped for SQL}
}
\details{

  res <- RMonetUnQuote("\\' escaped\& string \\t")
  print(res)

}

\note{
  Not much to note here.
}

\value{
  Returns true or false
}

\examples{\dontrun{
# un escape quote escaped string for SQL
RMonetUnQuote("\\' escaped\& string \\t")
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
