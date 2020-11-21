library(logger)
library(rvest)
library(dplyr)
library(fs)
source("setup.R")
source("process_local_tar_files.R")
source("scrape_download_files.R")
root <- 'https://cloud.r-project.org'
source("retrieve_deep_archive.R")
### settings.
current_date <- as.character(Sys.Date())
log_appender(appender_tee(paste0("data/log-",current_date,".txt")))
log_threshold(INFO)
log_info('download and extract Script start ')


available_packages_by_date <- paste0(root,"/web/packages/available_packages_by_date.html")
templocation <- "crandump"
CRANEXTRACT <- "CRAN"
agent <- httr::user_agent("scraping_cran R.M. Hogervorst rmhogervorst.nl")


# retrieve all active files
create_folders()

## check date against earlier run.
## or use available.packages
pkg_available <- available.packages()
check_against <- paste0(pkg_available[,"Package"], "_", pkg_available[,"Version"])
all_active_packages <- list()
all_active_packages$pkg <- check_against[!check_against %in% check_local_packages()]
if(length(all_active_packages$pkg)>0){
    all_active_packages$url <- paste0(root,"/src/contrib/",all_active_packages$pkg, ".tar.gz")
    purrr::walk(all_active_packages$url, download_and_extract)
}



## 
#all_active_packages <- scrape_page(available_packages_by_date)
# save file with date


# log_info("extracting tar.gz links, this will take a while.")
# ## consider furrr here.
# all_download_links <- purrr::map_chr(all_active_packages$url, find_source_on_package_page)
# log_info("found {length(all_download_links)} download links")
# package_versions <- purrr::map_chr(all_download_links, extract_p_name_from_url)
# # write away download links and extracted package version.
# write.csv(data.frame(
#     packages = package_versions,
#     url = all_download_links,
#     stringsAsFactors = FALSE
# ), file = paste0("data/downloadlinks", current_date,".csv"))
# write.csv(all_active_packages, paste0("data/active_packages", current_date,".csv"))
# 
# ### Download all information
# local_packages <- paste0(check_local_packages(),".tar.gz")
# download_links <- all_download_links[!package_versions %in% local_packages]
# purrr::walk(download_links, download_and_extract)



### go through archive page too. ##################
archive_packages <- retrieve_archive_links()
# save this file 
# 

# Unfortunately this requires us to go through every page in the 
# archive, to retrieve every link to a package version and collect
# all that into a list. There is no way to do this faster, because we don't 
# know when it was changed. (there is some informaiton in the page, we could use.)
all_archived_packages <- purrr::map(archive_packages, find_all_archived_on_package_page)

# download all archive packages and process
archival_packages_links <- unlist(all_archived_packages) #flatten the list into a vector
local_packages <- paste0(check_local_packages(),".tar.gz")

archive_download <- archival_packages_links[!archival_packages_links %>% extract_p_name_from_url() %in% local_packages]
archive_download <- archive_download[archive_download != "NA"]

log_info("archive to download reduced from {length(archival_packages_links)} to {length(archive_download)}")
purrr::walk(archive_download, download_and_extract)


#write out run time, and packages, so changes are captured, and more efficient crawl
#
log_info('download and extract Script stop ')
