### refresh author table
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
source("sqlite_setup.R")
source("info_to_db.R")
source("extract_namespace.R")
source("description_parser.R")

# DBI::dbExecute(con, "drop table authors;")
# 
CRANEXTRACT <- "CRAN"
current_date <- as.character(Sys.Date())
log_appender(appender_tee(paste0("data/refresh_authors-",current_date,".txt")))
log_threshold(INFO)
log_info('refresh authors start ')
create_tables_when_necessary()
all_packages <- as.character(fs::dir_ls(CRANEXTRACT))
all_packages <- all_packages[str_detect(all_packages,"_")]
log_info("there are currently {length(all_packages)} packages on disk")

only_refresh_authors <- function(package_folder){
    if(!fs::file_exists(paste0(package_folder,"/DESCRIPTION"))){
        log_warn("WARNING: no DESCRIPTION file in folder{package_folder}")
    }else{
        flattened_pkg <- 
            readLines(paste0(package_folder,"/DESCRIPTION"),warn = FALSE) %>% 
            flatten_pkg_description() %>% 
            str_replace_all("\\s", " ") %>% 
            str_replace_all("  ", " ") %>% 
            remove_empty_line()
        #all_field_names <- get_all_field_names(flattened_pkg)
        #fields_left <- all_field_names[!all_field_names %in% parsed_fields]
        key <- extract_key(flattened_pkg) 
        s_dump_authors(key, flattened_pkg)
    }
}

authors_done <- retrieve_keys_authors_table()
to_parse_description <- all_packages[!all_packages %in% paste0(CRANEXTRACT,"/",authors_done)]
log_info(" refreshing {length(to_parse_description)} author info of {length(all_packages)} total pkgs")
purrr::walk(to_parse_description, only_refresh_authors)

log_info('Refresh authors script stop ')
DBI::dbDisconnect(con)
