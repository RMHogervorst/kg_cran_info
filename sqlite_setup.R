### sets up db
##
##
create_tables_when_necessary <- function(){
    current_tables <- dbListTables(con)    
    
    # create tables, indexes
    required_tables <-c(
        "packages","hashes","exports","imports","namespace_handled", 
        "package_date", "authors", "dependencies_description",
        "package_other","package_meta"
        )
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
    if("package_date" %in% left){
        dbExecute(con, paste0(
            "CREATE TABLE IF NOT EXISTS package_date ",
            "(key CHAR(70) PRIMARY KEY NOT NULL, ",
            "datetime datetime",
            ");"
        ))
    }
    if("authors" %in% left){
        dbExecute(con, paste0(
            "CREATE TABLE IF NOT EXISTS authors ",
            "(key CHAR(70) NOT NULL, ",
            "given CHAR(150), ",
            "family CHAR(150), ",
            "email CHAR(150), ",
            "role CHAR(5) NOT NULL, ",
            "comment text, ",
            "ORCID CHAR(20) ",
            ");"
        ))
        dbExecute(con, "CREATE INDEX IF NOT EXISTS authors_idx on authors (key)")
        dbExecute(con, "CREATE INDEX IF NOT EXISTS orcid_idx on authors (ORCID)")
    }
    if("dependencies_description" %in% left){
        dbExecute(con, paste0(
            "CREATE TABLE IF NOT EXISTS dependencies_description ",
            "(key CHAR(70) NOT NULL, ",
            "package CHAR(70), ",
            "version CHAR(20), ",
            "package_exact CHAR(100), ",
            "type CHAR(20) ",
            ");"
        ))
        dbExecute(con, "CREATE INDEX IF NOT EXISTS dependencies_description_idx on dependencies_description (key)")
        dbExecute(con, "CREATE INDEX IF NOT EXISTS package_idx on dependencies_description (package)")
    }
    if("package_meta" %in% left){
        dbExecute(con, paste0(
            "CREATE TABLE IF NOT EXISTS package_meta (",
            "key CHAR(70) PRIMARY KEY NOT NULL, ",
            "packaged_datetime TEXT, ",
            "publication TEXT, ",
            "license CHAR(50), ",
            "needscompilation CHAR(20), ",
            "repository TEXT,",
            "lazydata TEXT",
            ");"
            ))
    }
    if("package_other" %in% left){
        dbExecute(con, paste0(
            "CREATE TABLE IF NOT EXISTS package_other (",
            "key CHAR(70) NOT NULL, ",
            "field TEXT, ",
            "value TEXT ",
            ");"
        ))
        dbExecute(con, "CREATE INDEX IF NOT EXISTS package_other_idx on package_other (key)")
    }
    
}
