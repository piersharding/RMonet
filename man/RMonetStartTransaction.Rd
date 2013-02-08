\name{RMonetStartTransaction}
\alias{RMonetStartTransaction}
\title{Start a SQL transaction}
\description{
  Start an SQL transaction with "START TRANSACTION"
}
\usage{
RMonetStartTransaction(con)
}%"
\arguments{
  \item{con}{an Open MonetDB Connection handle}
}
\details{

  con <- RMonetConnect("db.yml")
  RMonetStartTransaction(con)
  ...
  RMonetClose(con)

}

\note{
  Not much to note here.
}

\value{
  Returns true or false
}

\examples{\dontrun{
# Start an SQL transaction
RMonetStartTransaction(con)
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
