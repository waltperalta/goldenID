drop <- function() {
    con <- dbConnect(odbc(),
                     Driver = "SQL Server",
                     Server = "mandc-sql-01",
                     Database = "SEE_REP",
                     Trusted_Connection = "True")

    prompt <- readline(prompt = "Table to drop: ")
    drop <- paste0("DROP TABLE ", prompt)
    return(dbGetQuery(con, drop))
}
