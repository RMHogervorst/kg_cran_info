## Main script 
## 
## 


##### libraries
library(DBI)
library(RSQLite)
library(logger)
library(magrittr)
library(stringr)

### prerequisites.
if(!dir.exists("export")){
    dir.create("export")
}
con <- DBI::dbConnect(RSQLite::SQLite(),"export/database.db")
#### functions 
source("sqlite_setup.R")
source("info_to_db.R")
source("extract_namespace.R")
#### settings 
CRANEXTRACT <- "CRAN"
current_date <- as.character(Sys.Date())
log_appender(appender_tee(paste0("data/intodb-",current_date,".txt")))
log_threshold(INFO)
log_info('files to db Script start ')
create_tables_when_necessary()
all_packages <- as.character(fs::dir_ls(CRANEXTRACT))

####  INFO to db
hash_done <-paste0(CRANEXTRACT, "/",retrieve_keys_hash_table())
if(length(hash_done)==length(all_packages)){
    log_info("All hashes done, skipping hash insertion")
}else{
    log_info("parsing info: skipping {length(hash_done)} packages of total {length(all_packages)}")
    purrr::walk(.x = all_packages[!all_packages %in% hash_done],
                .f = drop_pkg_info_in_db)
}

#### parse namespace
namespace_done <- paste0(CRANEXTRACT, "/", retrieve_keys_imports_table())
if(length(namespace_done) == length(all_packages)){
    log_info("All namespace files done, skipping namespace insertion")
}else{
    log_info("parsing NAMESPACE: skipping {length(namespace_done)} packages of total {length(all_packages)}")
    purrr::walk(
        all_packages[!all_packages %in% namespace_done],
        drop_pkg_namespace_into_db
    )    
}

log_info('files to db Script stop ')
dbDisconnect(con)
