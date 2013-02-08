test.connecting <- function()
{
    conn <- RMonetConnect("db.yml")
    checkTrue(typeof(attr(conn, 'handle_ptr')) == 'externalptr')
    checkTrue(!is.null(attr(conn, 'handle_ptr')))
    info <- RMonetGetInfo(conn)
    #print(info)
    checkEquals(info[['host']], "localhost")
    checkEquals(info[['uri']], "mapi:monetdb://localhost:50000/voc")
    checkEquals(info[['lang']], "sql")
    checkEquals(info[['user']], "voc")
    checkEquals(info[['dbname']], "voc")
    checkTrue(RMonetClose(conn))

    conn <- RMonetConnect(dbhost='localhost', dbuser='voc', dbpass='voc', dbname='voc')

    checkTrue(typeof(attr(conn, 'handle_ptr')) == 'externalptr')
    checkTrue(!is.null(attr(conn, 'handle_ptr')))
    info <- RMonetGetInfo(conn)
    #str(info)
    checkEquals(info[['host']], "localhost")
    checkEquals(info[['uri']], "mapi:monetdb://localhost:50000/voc")
    checkEquals(info[['lang']], "sql")
    checkEquals(info[['user']], "voc")
    checkEquals(info[['dbname']], "voc")
    checkTrue(RMonetClose(conn))
}
           
#test.deactivation <- function()
#{
# DEACTIVATED('Deactivating this test function')
#}
