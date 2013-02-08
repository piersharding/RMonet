\name{RMonet-package}
\alias{RMonet}
\title{MonetDB SQL Interface}

\description{
  Package \pkg{RMonet} MonetDB SQL Interface implemented using the MAPI client library.

  See the package manual for details of installation and use.  (This
  will show up as a vignette, and can be accessed \emph{via}
  \code{RShowDoc("RMonet", package="RMonet")}.)
}

\details{
  Read and write to tables in the MonetDB in-memory column store DB, using
  SQL over the MonetDB MAPI client library

  con <- RMonetConnect("db.yml")

  or 

  con <- RMonetConnect(dbhost="localhost",
                          dbuser="voc",
                          dbpass="voc",
                          dbname="voc")

  info = RMonetGetInfo(con)
  print(info)

  parms <- list(3, 50)
  qry <- "SELECT * FROM voc.craftsmen WHERE trip > ? LIMIT ?"

  res <- RMonetQuery(con, qry, parms)
  print(res)
  # alternative shorthand
  res <- dbquery(con, qry, parms)
  print(res)
  RMonetClose(con)

  All Query results are returned as data.frame.
}

\note{
  Please see the README and INSTALL file that come with RMonet - these
  will give the latest info on how to install and test the package, 
  inparticular details are given on how to setup the test database and 
  run the test suite.
}

\author{
  Piers Harding
}
\seealso{
  \code{\link{RMonet}}, \code{\link{RMonetConnect}}, \code{\link{RMonetGetInfo}}, \code{\link{RMonetQuery}}, \code{\link{RMonetLoadDataFrame}}, \code{\link{RMonetExecute}}, \code{\link{RMonetClose}}, \code{\link{RMonetStartTransaction}}, \code{\link{RMonetCommit}}, \code{\link{RMonetRollback}}, \code{\link{RMonetIsConnected}}, \code{\link{RMonetQuote}}, \code{\link{RMonetUnQuote}}, \code{\link{RMonetExists}}, \code{\link{RMonetListTables}}, \code{\link{RMonetListFields}}
}
\keyword{IO}
\keyword{SQL}
\keyword{MonetDB}
