\name{RMonetConnect}
\alias{RMonetConnect}
\title{RMonet Open Connections to the database}
\description{
  Open connections to a MonetDB database
}
\usage{
RMonetConnect(...)

}%"
\arguments{
  \item{...}{all MonetDB connection parameters for the MAPI client library}
}
\details{
  \code{RMonetConnect} establishes an open database connection to the specified MonetDB
  There are two styles of passing the connection parameters:
  - \code{RMonetConnect('db.yml')} where the name of A YAML encoded file containing
    MonetDB connection parameters is passed in
  - \code{RMonetConnect(dbhost='localhost', dbuser='voc', dbpass='voc', dbname='voc', ...)}
    The individual connection parameters are passed as per the requirements of the MonetDB mapi library
    These parameters are typically:
     * dbhost - the host and socket connection string/uri of MonetDB
     * dbuser - MonetDB user name
     * dbpass - user password
     * dbname - MonetDB name

}

\note{
  Not much to note here.
}

\value{
  Returns an object that contains the MonetDB connection object that you can
  then use to pass to RMonetClose, RMonetQuery, and RMonetGetInfo.
}
\examples{\dontrun{
# full parameter specification
con <- RMonetConnect(dbhost='localhost', dbuser='voc', dbpass='voc', dbname='voc')

or

# Use a YAML encoded parameter file
con <- RMonetConnect("db.yml")

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
