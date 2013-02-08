\name{RMonetExists}
\alias{RMonetExists}
\alias{readCube}
\title{Check table exists}
\description{
  Run a check to see if a table exists
}
\usage{
RMonetExists(con, tablename)
}%"
\arguments{
  \item{con}{an Open MonetDB Connection handle}
  \item{tablename}{The name of a table to check}
}
\details{

  \preformatted{
  con = RMonetConnect("db.yml")

  res <- RMonetExists(con, "craftsmen")
  print(res)

  RMonetClose(con)
  }
}

\note{
  nothing
}

\value{
  Returns boolean true or false depending on whether the table exists or not
}
\examples{\dontrun{
# Check if the voc.craftsmen table exists
  res <- RMonetExists(con, "craftsmen")
  print(res)
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
