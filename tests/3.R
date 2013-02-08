test.query_table_noparms <- function()
{
    conn <- RMonetConnect("db.yml")
    res = RMonetQuery(conn, "SELECT * FROM voc.craftsmen LIMIT 3")
    #print(res)
    #print(res$number[1])
    checkTrue(length(res$trip) == 3)
    checkEquals(1376, res$number[1])
    checkTrue(RMonetClose(conn))
}
