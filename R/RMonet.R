# file RMonet/R/RMonet.R
# copyright (C) 2012 and onwards, Piers Harding
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 or 3 of the License
#  (at your option).
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  A copy of the GNU General Public License is available at
#  http://www.r-project.org/Licenses/
#
#  Function library of mapi integration for MonetDB
#
#
#
.onLoad <- function(libname, pkgname)
{
    if(is.null(getOption("dec")))
        options(dec = Sys.localeconv()["decimal_point"])
}

.onUnload <- function(libpath)
{
    .Call(C_RMonetTerm)
    library.dynam.unload("RMonet", libpath)
}


RMonetConnect <- function (...)
{
    args <- list(...)
    if (length(args) == 0) {
        stop("No arguments supplied")
    }
    if (typeof(args[[1]]) == "list") {
        args = args[[1]]
    }
    # did we get passed a config file?
    if (typeof(args[[1]]) == "character" && file.exists(args[[1]])) {
        library(yaml)
        config <- yaml.load_file(args[[1]])
        newargs <- list()
        for (x in names(config)) { newargs[[x]] <- as.character(config[[x]]); }
        return(RMonetConnect(newargs))
    }
    # format dbport
    if (exists("dbport", where=args)) {
        args[['dbport']] <- sprintf("%05d", as.integer(args[['dbport']]))
    }
    res <- .Call(C_RMonetConnect, args)
    return(res)
}

RMonetshowArgs <-
    function(...)
{
    res <- .Call(C_RMonetshowArgs, list(...))
    return(res)
}

RMonetIsConnected <-  function(con)
{
    if (!is.integer(con)) {
        #print("RMonetIsConnected: handle is not an integer")
        return(FALSE)
    }
    if (!is.element("handle_ptr", names(attributes(con)))) {
        #print("RMonetIsConnected: handle_ptr does not exist")
        return(FALSE)
    }
    res <- .Call(C_RMonetIsConnected, con)
    return(res)
}


RMonetGetInfo <- function(con)
{
    if(!RMonetIsConnected(con))
       stop("RMonetGetInfo: argument is not a valid RMonet handle")
    res <- .Call(C_RMonetGetInfo, con)
    return(res)
}


RMonetExecute <- function(con, sql, autocommit=TRUE, lastid=FALSE, try=FALSE)
{
    if(!RMonetIsConnected(con))
       stop("argument is not a valid RMonet handle")
    res <- .Call(C_RMonetExecute, con, sql, autocommit, lastid, try)
    return(res)
}


RMonetStartTransaction <- function(con)
{
    if(!RMonetIsConnected(con))
       stop("argument is not a valid RMonet handle")
	res <- RMonetExecute(con, "START TRANSACTION", autocommit=TRUE)
    return(res)
}


RMonetCommit <- function(con)
{
    if(!RMonetIsConnected(con))
       stop("argument is not a valid RMonet handle")
	res <- RMonetExecute(con, "COMMIT", autocommit=FALSE)
    return(res)
}


RMonetRollback <- function(con)
{
    if(!RMonetIsConnected(con))
       stop("argument is not a valid RMonet handle")
	res <- RMonetExecute(con, "ROLLBACK", autocommit=FALSE)
    return(res)
}


RMonetExists <- function(con, tablename)
{
    if(!RMonetIsConnected(con))
       stop("argument is not a valid RMonet handle")
    exists <- paste('SELECT * FROM "', tablename, '" WHERE 1=0', sep="")
	res <- RMonetExecute(con, exists, autocommit=FALSE, lastid=FALSE, try=TRUE)
    return(res)
}


RMonetListTables <- function(con)
{
    if(!RMonetIsConnected(con))
       stop("argument is not a valid RMonet handle")
    sql <- "SELECT \"o\".\"name\" AS name, (CASE \"o\".\"type\" WHEN 0 THEN 'TABLE' WHEN 1 THEN 'VIEW' ELSE '' END) AS \"type\", \"o\".\"query\" AS query FROM \"sys\".\"_tables\" o, \"sys\".\"schemas\" s WHERE \"o\".\"schema_id\" = \"s\".\"id\" AND \"o\".\"type\" IN (0,1) AND \"s\".\"name\" = \"current_schema\""
	res <- RMonetQuery(con, sql, autocommit=FALSE)
    return(res)
}


RMonetListFields <- function(con, tablename, schema="")
{
    if(!RMonetIsConnected(con))
       stop("argument is not a valid RMonet handle")
    if (nchar(schema) > 0) {
        schema <- paste(" AND \"s\".\"name\" = '", schema, "' ", sep="")
    }
    else {
        schema <- ""
    }
    #sql <- paste("SELECT * FROM \"", tablename, "\" LIMIT 1", sep="")
    sql <- paste("SELECT \"s\".\"name\" AS \"schema\", \"t\".\"name\" AS \"table\", \"c\".\"name\" AS \"column\", \"c\".\"type\", \"c\".\"type_digits\", \"c\".\"type_scale\", \"c\".\"null\", \"c\".\"default\", \"c\".\"number\" FROM \"sys\".\"_columns\" \"c\", \"sys\".\"_tables\" \"t\", \"sys\".\"schemas\" \"s\" WHERE \"c\".\"table_id\" = \"t\".\"id\" AND '", tablename, "' = \"t\".\"name\"", schema, " AND \"t\".\"schema_id\" = \"s\".\"id\" ORDER BY \"schema\", \"number\"", sep="")
	res <- RMonetQuery(con, sql, autocommit=FALSE)
    return(res)
}


RMonetQuery <- function(con, sql, parameters=list(), autocommit=FALSE, lastid=FALSE, page=FALSE)
{
    if(!RMonetIsConnected(con))
       stop("argument is not a valid RMonet handle")
	parameters <- as.list(as.character(parameters))
    res <- .Call(C_RMonetQuery, con, sql, parameters, autocommit, lastid, page)
    return(res)
}

rmonet_col <- function(name, typ, len, fac)
{
    if (typ == 'integer') {
        if (fac) {
            if (len < 20) {
                len = 20
            }
            else if (len < 50) {
                len = 50
            }
            else if (len < 100) {
                len = 100
            }
            typ = paste('varchar(', len, ')', sep='')
        }
        else {
            typ = 'integer'
        }
    }
    else if (typ == 'numeric' || typ == 'double') {
        typ = 'real'
    }
    else {
        if (len == 1) {
            len = 1
        }
        else if (len < 5) {
            len = 5
        }
        else if (len < 10) {
            len = 10
        }
        else if (len < 20) {
            len = 20
        }
        else if (len < 50) {
            len = 50
        }
        else if (len < 100) {
            len = 100
        }
        typ = paste('varchar(', len, ')', sep='')
    }
    return(paste('"', name,'" ', typ, ' DEFAULT NULL', sep=''))
}


rmonet_val <- function(val)
{
    if (is.na(val)) {
        return('NULL')
    }
    typ <- typeof(val)
    if (typ == 'integer') {
        if (is.factor(val)) {
            val = paste('"', as.character(val), '"', sep='')
        }
        else {
            val = sprintf("%d", val)
        }
    }
    else if (typ == 'numeric' || typ == 'double') {
        val = as.character(val)
    }
    else {
        val = paste('"', RMonetQuote(val), '"', sep='')
    }
    return(val)
}


RMonetLoadDataFrame <- function(con, frame, tablename, drop=FALSE, chunk=500000, tmp.file=FALSE, append=FALSE)
{
    if(!RMonetIsConnected(con))
       stop("argument is not a valid RMonet handle")
	frame <- as.data.frame(frame)
	tablename <- as.character(tablename)
    cols <- paste(lapply(names(frame), FUN=function (x) {return(rmonet_col(x, typeof(frame[[x]]), max(unlist(lapply(as.character(frame[[x]]), FUN=nchar))), is.factor(frame[[x]])))}), collapse=", ")
    create <- paste('CREATE TABLE "', tablename, '" (', cols, ')', sep="") 
    there <- RMonetExists(con, tablename)
    # wrap the whole thing in a single transaction
    RMonetStartTransaction(con)
    if (append) {
        if (!there)
            stop("table must exist in append mode")
    }
    else {
        if (drop) {
            # check if table exists first
            if (there) {
                delete <- paste('DROP TABLE "', tablename, '"', sep="")
	            res <- RMonetExecute(con, delete, autocommit=FALSE)
            }
        }

	    res <- RMonetExecute(con, create, autocommit=FALSE)
    }

    # calculate the values for the rows
    rows <- length(frame[[1]])

    # must have some rows
    if (rows == 0) {
        return(res)
    }

    if (tmp.file) {
        tmpfile <- paste("/tmp/", tablename,".tmp.csv", sep="")
        query <- paste("COPY ", sprintf("%0d", rows), ' RECORDS INTO "', tablename, '" FROM stdin USING DELIMITERS \'\\t\',\'\\n\',\'"\';', "\n", sep="")
        cat(query, file=tmpfile)
        write.table(frame, file=tmpfile, sep="\t", na='NULL', row.names=FALSE, col.names=FALSE, append=TRUE, eol="\n")
        cat("\n", file=tmpfile, fill=FALSE, append=TRUE)
        query <- paste(readLines(tmpfile), collapse="\n")
        #print(query)
	    tot <- RMonetExecute(con, query, autocommit=FALSE)
    }
    else {
        # work out how many chunks
        iter <- ceiling(rows/chunk)
        tot <- 0

        for (j in 1:iter) {
            start = ((j - 1) * chunk) + 1
            end = j * chunk
            if (end > rows)
                end = rows
            #print(paste("start: ", start))
            #print(paste("end: ", end))
            #query <- c(paste("COPY ", sprintf("%0d", (end - start + 1)), ' RECORDS INTO "', tablename, '" FROM stdin USING DELIMITERS \'\\t\',\'\\n\',\'"\';', "\n", sep=""))
            out <- textConnection(NULL, open="a")
            cat(paste("COPY ", sprintf("%0d", (end - start + 1)), ' RECORDS INTO "', tablename, '" FROM stdin USING DELIMITERS \'\\t\',\'\\n\',\'"\';', "\n", sep=""), file=out, fill=FALSE, append=TRUE)
            #for (i in start:end) {
                #query <- append(query, paste(paste(lapply(table[i,], FUN=rmonet_val), collapse="\t"), "\n", sep=""))
            write.table(frame[start:end,], file=out, sep="\t", na='NULL', row.names=FALSE, col.names=FALSE, append=TRUE, eol="\n")
            #}
            cat("\n", file=out, fill=FALSE, append=TRUE)
            query <- paste(textConnectionValue(out), collapse="\n")
            close(out)
	        res <- RMonetExecute(con, query, autocommit=FALSE)
            tot = tot + res
        }
    }
    RMonetCommit(con)

    return(tot)
}


RMonetQuote <- function(str)
{
    res <- .Call(C_RMonetQuote, str)
    return(res)
}


RMonetUnQuote <- function(str)
{
    res <- .Call(C_RMonetUnQuote, str)
    return(res)
}

print.RMonet_Connector <- function(x, ...) RMonetGetInfo(x)

close.RMonet_Connector <- function(con, ...) RMonetClose(con)

RMonetClose <- function(con)
{
    if(!RMonetIsConnected(con))
       stop("argument is not a connected RMonet handle")
    res <- .Call(C_RMonetClose, con)
    return(res)
}

if (!exists("dbwrite")) {
    dbwrite <- function(con, ...){
              res <-  RMonetLoadDataFrame(con, ...)
              return(res)
          }
}

if (!exists("dbquery")) {
    dbquery <- function(con, ...){
              res <-  RMonetQuery(con, ...)
              return(res)
          }
}

if (!exists("dbexecute")) {
    dbexecute <- function(con, ...){
              res <-  RMonetExecute(con, ...)
              return(res)
          }
}

#setClass("RMonet_Connector")
#setMethod("dbwrite", "RMonet_Connector",
#          def = function(con, ...){
#              res <-  RMonetLoadDataFrame(con, ...)
#              return(res)
#          },
#          valueClass = "data.frame"
#        )
#setMethod("dbquery", "RMonet_Connector",
#          def = function(con, ...){
#              res <-  RMonetQuery(con, ...)
#              return(res)
#          },
#          valueClass = "data.frame"
#        )
#setMethod("dbexecute", "RMonet_Connector",
#          def = function(con, ...){
#              res <-  RMonetExecute(con, ...)
#              return(res)
#          },
#          valueClass = "data.frame"
#        )

