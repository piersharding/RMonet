\name{RMonetLoadDataFrame}
\alias{RMonetLoadDataFrame}
\alias{dbwrite}
\title{Load a data.frame into a MonetDB table}
\description{
  Load a data.frame object into a MonetDB table with automatic schema detection
}
\usage{
RMonetLoadDataFrame(con, frame, tablename, drop=FALSE, chunk=500000, tmp.file = FALSE, append = FALSE)
}%"
\arguments{
  \item{con}{an Open MonetDB Connection handle}
  \item{frame}{The data.frame to be loaded into a MonetDB table}
  \item{tablename}{The table name to load the data.frame into}
  \item{drop}{optional - flag to indicate to drop the table if it exists before recreating on the load}
  \item{chunk}{optional - the number of records to process in each block - default 500,000}
  \item{tmp.file}{optional - flag to use write.csv and temp file instead of memory for table contents formatting - useful for largedata.frame processing}
  \item{append}{optional - flag to indicate appending dta.frame to an existing table}
}
\details{

  con <- RMonetConnect("db.yml")
  res <- RMonetLoadDataFrame(con, craftsmen, "tmp_craftsmen")
  print(res)
  RMonetClose(con)

}

\note{
  Not much to note here.
}

\value{
  Returns loaded row count - should match the data.frame passed in
}

\examples{\dontrun{
# Close the connection
RMonetLoadDataFrame(con, my.frame, "tmp_tab", drop=TRUE)
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
