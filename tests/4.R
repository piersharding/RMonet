test.execute_query_check_rows <- function()
{
    conn <- RMonetConnect("db.yml")
    res = RMonetExecute(conn, "SELECT * FROM voc.craftsmen LIMIT 3")
    #print(res)
    checkTrue(as.numeric(res) == 3)
    checkTrue(RMonetClose(conn))
}
