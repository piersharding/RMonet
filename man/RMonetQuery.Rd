\name{RMonetQuery}
\alias{RMonetQuery}
\alias{dbquery}
\title{Query MonetDB using SQL}
\description{
  Run arbitrary SQL queries against the MonetDB and return the results as a data.frame
}
\usage{
RMonetQuery(con, sql, parameters=list(), autocommit=FALSE, lastid=FALSE, page=FALSE)
}%"
\arguments{
  \item{con}{an Open MonetDB Connection handle}
  \item{sql}{Arbitrary SQL to execute}
  \item{parameters}{list of string values substituted into the ? placeholders}
  \item{autocommit}{optional - boolean flag TRUE to auto commit}
  \item{lastid}{optional - boolean flag TRUE to return the last row ID affected - good for INSERT statements}
  \item{page}{optional - boolean flag TRUE to page the results - does nothing at the moment :-)}
}
\details{
  \preformatted{
  con <- RMonetConnect("db.yml")
  res <- RMonetQuery(con,  "SELECT * FROM voc.craftsmen WHERE trip > ? LIMIT ?", list(3,15))
  print(res)

  RMonetClose(con)
  }

}

\note{
  Not much to note here.
}

\value{
  Returns a data.frame of the table contents selected
}
\examples{\dontrun{
# read the VOC craftsmen table with parameter substitutions
RMonetQuery(con,  "SELECT * FROM voc.craftsmen WHERE trip > ? LIMIT ?", list(3,15))
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
