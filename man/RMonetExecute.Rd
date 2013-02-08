\name{RMonetExecute}
\alias{RMonetExecute}
\alias{dbexecute}
\title{Execute SQL against MonetDB}
\description{
  Execute arbitrary SQL statements against the MonetDB database
}
\usage{
RMonetExecute(con, sql, autocommit=TRUE, lastid=FALSE, try=FALSE)
}%"
\arguments{
  \item{con}{an Open MonetDB Connection handle}
  \item{sql}{SQL query to call}
  \item{autocommit}{optional - set autocommit for the statement to true}
  \item{lastid}{optional - return the record ID of the record affected - useful for INSERT}
  \item{try}{optional - try executing the statement - return FALSE on failure - TRUE for success}
}
\details{

  con <- RMonetConnect("db.yml")
  res = RMonetExecute(con, "SELECT * FROM voc.craftsmen LIMIT 3")
  print(res)
  checkTrue(as.numeric(res) == 3)
  RMonetClose(con)

}

\note{
  nothing.
}

\value{
  RMonetExecute executes and returns the count of records affected 
}

\examples{\dontrun{
# Execute the query
RMonetExecute(con, "SELECT * FROM voc.craftsmen LIMIT 3")
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
