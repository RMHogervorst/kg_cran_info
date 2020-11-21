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




