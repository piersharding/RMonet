\name{RMonetCommit}
\alias{RMonetCommit}
\title{execute a COMMIT transaction call}
\description{
  Execute an SQL COMMIT transaction call.
}
\usage{
RMonetCommit(con)
}%"
\arguments{
  \item{con}{an Open MonetDB Connection handle}
}
\details{

  RMonetCommit(con)

}

\note{
  Not much to note here.
}

\value{
  Returns true or false
}
\examples{\dontrun{
# Commit the open transaction
RMonetCommit(con)
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
