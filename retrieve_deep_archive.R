# retrieve_all_archive

# https://cran.r-project.org/src/contrib/Archive/
archive_page <- paste0(root,"/src/contrib/Archive/")

# list_all_packages 
# 
# go into each page
# extract the source files.
retrieve_archive_links <- function(){
    log_info("Extracting full list of archive")
    links <- 
        read_html(archive_page) %>% 
        html_nodes("a") %>% 
        html_attr("href")
    none <- !stringr::str_detect(links,"/src/contrib")
    questionmarks <-  !stringr::str_starts(links,"\\?")
    
    paste0(archive_page, links[questionmarks & none])
}


find_all_archived_on_package_page <- function(page){
    log_debug("searching archive for {page}")
    httr::with_config(config = agent,{
        links <- read_html(page) %>% 
            html_nodes("a") %>% 
            html_attr('href')
    } )
    
    archive <- links %>% stringr::str_detect(".tar.gz$")
    result <- paste0(page, links[archive])
    log_info("found {length(result)} package versions in archive")
    result
}

find_all_archived_dates <- function(page){
    log_debug("searching archive for {page}")
    httr::with_config(config = agent,{
        text_example <- 
            read_html(page) %>% 
            html_node("pre") %>% 
            html_text(trim = TRUE) %>% 
            strsplit(split="\n") %>% 
            unlist()
    } )
    pkg_archive_links <- text_example[stringr::str_detect(text_example,"tar.gz")] %>% stringr::str_trim(side="both")
    pkgs <- pkg_archive_links %>% stringr::str_extract_all("[A-z0-9._-]+\\.tar\\.gz") %>% unlist() %>% trimws(which="both")
    if(length(pkgs)>0){
        date_time <- pkg_archive_links %>% stringr::str_extract_all("[0-9]{4}-[0-9]{1,2}-[0-9]{1,2} [0-9]{1,2}:[0-9]{2}") %>% unlist()
        data.frame(
            pkg = pkgs,
            datetime=date_time
        ) %>% readr::write_csv(file = "data/archive_dates.csv",
                               append = TRUE)
    }else{
        log_warn("No pkg_archive links found for {page}")
    }
}

readr::write_csv(data.frame(pkg=character(0), datetime=character(0)), file = "data/archive_dates.csv")



