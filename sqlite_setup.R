### sets up db
##
##
create_tables_when_necessary <- function(){
    current_tables <- dbListTables(con)    
    
    # create tables, indexes
    required_tables <-c("packages","hashes","exports","imports","namespace_handled")
    left <- required_tables[!required_tables %in% current_tables]
    log_info("creating tables {left}")
    #### logic
    if("packages" %in% left){
        dbExecute(con,
                  paste0(
                      "CREATE TABLE IF NOT EXISTS packages", 
                      "(key CHAR(70) PRIMARY KEY NOT NULL, ",
                      "package CHAR(70) NOT NULL, ",
                      "version CHAR(70) NOT NULL, ",
                      "uploaddate DATETIME NOT NULL ",
                      ")"
                  ))
    }
    if("hashes" %in% left){
        dbExecute(con, paste0(
            "CREATE TABLE IF NOT EXISTS hashes", 
            "(key CHAR(70) PRIMARY KEY NOT NULL, ",
            "size INT ,",
            "sha1 CHAR(50) NOT NULL, ",
            "sha256 CHAR(70) NOT NULL, ",
            "sha512 CHAR(140) NOT NULL ",
            ")"
        ))
    }
    if("exports" %in% left){
        dbExecute(con, paste0(
            "CREATE TABLE IF NOT EXISTS exports", 
            "(key CHAR(70) NOT NULL, ",
            "fun CHAR(100), ",
            "type CHAR(30)",
            ")"
        ))
        dbExecute(con, "CREATE INDEX IF NOT EXISTS exports_idx on exports (key)")
    }
    if("imports" %in% left){
        dbExecute(con, paste0(
            "CREATE TABLE IF NOT EXISTS imports", 
            "(key CHAR(70) NOT NULL, ",
            "package CHAR(50), ",
            "fun CHAR(100), ",
            "type CHAR(30)",
            ")"))   
        dbExecute(con, "CREATE INDEX IF NOT EXISTS imports_idx on imports (key)")
    }
    if("namespace_handled" %in% left){
        dbExecute(con, paste0(
            "CREATE TABLE IF NOT EXISTS namespace_handled ",
            "(key CHAR(70) PRIMARY KEY NOT NULL, ",
            "namespace INT",
            ");"
        ))
    }
    
}
