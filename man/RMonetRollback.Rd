\name{RMonetRollback}
\alias{RMonetRollback}
\title{execute a ROLLBACK transaction call}
\description{
  Execute an SQL ROLLBACK transaction call.
}
\usage{
RMonetRollback(con)
}%"
\arguments{
  \item{con}{an Open MonetDB Connection handle}
}
\details{

  RMonetRollback(con)

}

\note{
  Not much to note here.
}

\value{
  Returns true or false
}
\examples{\dontrun{
# Rollback the open transaction
RMonetRollback(con)
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
