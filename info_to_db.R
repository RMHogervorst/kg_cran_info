### info to db

# folder <- paste0(CRANEXTRACT, "/A3_0.9.1")
drop_pkg_info_in_db <- function(folder){
    log_info("Parsing INFO in {sub(pattern='CRAN/',replacement='',x=folder)}")
    file <- readLines(paste0(folder,"/INFO"))
    name <- stringr::str_replace(file[1], "File information: (.+)$", "\\1")
    size <- as.integer(stringr::str_replace(file[2],"size=(.+)$", "\\1"))
    sha1 <- stringr::str_replace(file[4], "sha1=", "")
    sha256 <- stringr::str_replace(file[5], "sha256=", "")
    sha512 <- stringr::str_replace(file[6], "sha512=", "")
    
    dbAppendTable(con, name="hashes",
                  value=data.frame(
                      key = name,
                      size=size,
                      sha1=sha1,
                      sha256=sha256,
                      sha512=sha512)
                      )
    
}

retrieve_keys_hash_table <- function(){
    dbGetQuery(con, "SELECT key FROM hashes;")$key
}

drop_archive_pkg_dates_into_table <- function(df){
    names(df) <- c("key", "datetime")
    df <- df[!is.na(df$key),]
    df <- df[!df$key %in% retrieve_keys_package_date(),]
    if(nrow(df)>0){
        dbAppendTable(con, name="package_date",
                                    value=df)
        log_info("Wrote {nrow(df)} rows in table package_date")        
    }else{
        log_info("No new packages")
    }
}

retrieve_keys_package_date <- function(){
    dbGetQuery(con, "SELECT key FROM package_date;")$key
}
