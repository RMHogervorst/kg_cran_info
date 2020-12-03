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
source("description_parser.R")
#### settings 
CRANEXTRACT <- "CRAN"
current_date <- as.character(Sys.Date())
log_appender(appender_tee(paste0("data/intodb-",current_date,".txt")))
log_threshold(INFO)
log_info('files to db Script start ')
create_tables_when_necessary()
all_packages <- as.character(fs::dir_ls(CRANEXTRACT))
all_packages <- all_packages[str_detect(all_packages,"_")]
log_info("there are currently {length(all_packages)} packages on disk")
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
### Parse archive date info 
log_info("parsing data/archive_links.csv")
archive_dates <- readr::read_csv("data/archive_dates.csv")
drop_archive_pkg_dates_into_table(archive_dates)

### parse Description files

to_skip <- DESCRIPTION_all_done()
to_parse_description <- all_packages[!all_packages %in% paste0(CRANEXTRACT,"/",to_skip)]
#stopifnot(length(to_parse_description) + length(to_skip) == length(all_packages))

log_info(" parsing {length(to_parse_description)} package DESCRIPTIONs of {length(all_packages)} total pkgs")
purrr::walk(to_parse_description, parse_description_file)
# description_done 
# all_folders
## why do I have a folder CRAN/NA?

## stop
log_info('files to db Script stop ')
dbDisconnect(con)
