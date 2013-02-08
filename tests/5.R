test.read_schema <- function()
{
    conn <- RMonetConnect("db.yml")
    res = RMonetListTables(conn)
    #print(res)
    checkTrue(length(res$name) >= 10)
    res = RMonetListFields(conn, "voyages", schema="voc")
    checkEquals(22, length(res$column))
    #print(res)
    checkTrue(RMonetClose(conn))
}
