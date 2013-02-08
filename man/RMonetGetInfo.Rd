\name{RMonetGetInfo}
\alias{RMonetGetInfo}
\title{get connection details to MonetDB}
\description{
  get connection details regarding the nominated MonetDB connection
}
\usage{
RMonetGetInfo(con)
}%"
\arguments{
  \item{con}{an Open MonetDB Connection handle}
}
\details{
  \code{RMonetGetInfo} get connection details 
}

\note{
  Not much to note here.
}

\value{
  Returns a named list of details about the connection
}
\examples{\dontrun{
# Close the connection
info <- RMonetGetInfo(con)
print(info)
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
