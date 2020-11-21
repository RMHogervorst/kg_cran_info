### scrape and download function
scrape_page<- function(page){
    log_info(paste0("Starting all_active_packages scrape on ", page))
    httr::with_config(config = agent,{
        tablelist <- xml2::read_html(page)
    } )
    ## the next step is very slow!
    res <- tablelist %>% html_node("table") %>% html_table()
    stopifnot(all(names(res) == c('Date','Package','Title')))
    links <- tablelist %>% html_node("table") %>% html_nodes('a')
    
    link_df <- data.frame(
        url = links %>% html_attr('href') %>% stringr::str_replace("../..", root),
        Package = links %>% html_text()
    )
    result <- res %>% left_join(link_df)
    log_info(paste0("Found ",nrow(result)," active packages"))
    result
}
### Maybe check when latest run was

# f.e. page2 <- "https://cloud.r-project.org/web/packages/brglm2/index.html"
# Better make this safe to fail. na_char if failure
find_source_on_package_page <- function(page){
    log_debug("searching {page}")
    httr::with_config(config = agent,{
        links <- read_html(page) %>% 
            html_nodes('table') %>% .[2] %>% 
            html_nodes("a") %>% 
            html_attr('href')
    } )
    
    srcs <- links %>% stringr::str_detect("/src/contrib/")
    archive <- links %>% stringr::str_detect("/src/contrib/Archive")
    tarfile <- links[srcs & !archive]
    stopifnot(length(tarfile) ==1)
    tarfile %>% stringr::str_replace("../../..", root)
}
#download_link <- find_source_on_package_page(page2)

download_tarfile <- function(link, destfile){
    curl::curl_download(link, destfile = destfile)
}


