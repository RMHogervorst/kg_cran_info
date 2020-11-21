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

