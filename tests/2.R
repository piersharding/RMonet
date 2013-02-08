test.query_table_with_parms <- function()
{
    conn <- RMonetConnect("db.yml")
    res = RMonetQuery(conn, "SELECT * FROM voc.craftsmen WHERE trip > ? LIMIT ?", list(3,15))
    checkEquals(15, length(res$trip))
    checkTrue(RMonetClose(conn))
}
