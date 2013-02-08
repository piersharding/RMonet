\name{RMonetListFields}
\alias{RMonetListFields}
\title{List the fields of a table}
\description{
  List the fields of a MonetDB table
}
\usage{
RMonetListFields(con, tablename, schema="")
}%"
\arguments{
  \item{con}{an Open MonetDB Connection handle}
  \item{tablename}{The table name to get fields for}
  \item{schema}{optional - specify a schema if necessary}
}
\details{

  con <- RMonetConnect("db.yml")
  res <- RMonetListFields(con, "craftsmen")
  print(res)
  RMonetClose(con)

}

\note{
  Not much to note here.
}

\value{
  Returns a data.frame representing the fields and characteristics of a table
}
\examples{\dontrun{
# List fields
RMonetListFields(con, "craftsmen", schema="voc")
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
