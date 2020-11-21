### Export hash files
### 
#libraries
library(DBI)
library(readr)
library(logger)
### settings
con <- DBI::dbConnect(RSQLite::SQLite(),"export/database.db")
CRANEXTRACT <- "CRAN"
current_date <- as.character(Sys.Date())
###logging
log_appender(appender_tee(paste0("data/export_files-",current_date,".txt")))
log_threshold(INFO)
log_info('exports files Script start ')

hash_table <- dbGetQuery(con, "SELECT * FROM hashes")
size_tbl <- nrow(hash_table)
log_info("writing {size_tbl} sha1 hashes")
readr::write_csv(hash_table[,c("key","sha1")], file="export/rpgks_sha1.csv")

log_info("writing {size_tbl} sha256 hashes")
readr::write_csv(hash_table[,c("key","sha256")], file="export/rpgks_sha256.csv")

log_info("writing {size_tbl} sha512 hashes")
readr::write_csv(hash_table[,c("key","sha512")], file="export/rpgks_sha512.csv")

dbDisconnect(con)
