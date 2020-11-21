### process local tar files

# check if folder exists in CRAN/
# check if that folder contains info, NAMESPACE, DESCRIPTION, MAN/


FILES <- c("DESCRIPTION","NAMESPACE")


file_info <- function(file){
    list(
        name = sub(pattern = paste0(templocation, "/"), "", sub(pattern = ".tar.gz","", file)),
        size = file.size(file),
        sha1 = openssl::sha1(file),
        sha256 = openssl::sha256(file),
        sha512 = openssl::sha512(file)
    )
}

file_out <- function(file_info){
    paste(
        paste0("File information: ", file_info$name),
        paste0("size=", file_info$size),
        "##filehashes##",
        paste0( "sha1=", file_info$sha1),
        paste0( "sha256=", file_info$sha256),
        paste0( "sha512=", file_info$sha512),
        sep="\n"
    )
}


untar_files_and_extract_info <- function(file){
    fileinfo <- file_info(file)
    log_debug(paste0("Processing ",fileinfo$name))
    outdir <- paste0(CRANEXTRACT,"/",fileinfo$name)
    # create dir
    # write info
    fs::dir_create(outdir)
    fileinfo %>% 
        file_out() %>% 
        writeLines(con=paste0(outdir,"/INFO"))
    # unpack and copy files
    untar(file,exdir = paste0(templocation,"/"))
    untarred_dir<- paste0(templocation,"/",sub('_.*$',"",fileinfo$name))
    files_in <- paste0(untarred_dir,"/",FILES)
    files_out <- paste0(outdir,"/",FILES)
    if(fs::file_exists(files_in[[1]])){
        fs::file_copy(path = files_in[[1]], new_path = files_out[[1]],overwrite = TRUE)
    }
    if(fs::file_exists(files_in[[2]])){
        fs::file_copy(path = files_in[[2]], new_path = files_out[[2]],overwrite = TRUE)
    }
    if(fs::dir_exists(paste0(untarred_dir,"/man/"))){
        fs::dir_copy(
            path = paste0(untarred_dir,"/man/"),
            new_path = paste0(outdir,"/man/"),overwrite = TRUE
        )
    }
    # clean up
    fs::file_delete(file)
    fs::dir_delete(untarred_dir)
}
# doesn't work with
# https://cloud.r-project.org/src/contrib/Archive/A3/A3_0.9.1.tar.gz
# "https://cloud.r-project.org/src/contrib/Archive/aaMI/aaMI_1.0-0.tar.gz"
extract_p_name_from_url <- function(url){
    stringr::str_replace(url,paste0(root,"/src/contrib/"),"") %>% 
        stringr::str_extract("[A-Za-z.0-9_-]*$")
}

download_and_extract <- function(link){
    pkgname <- extract_p_name_from_url(link)
    log_info("Downloading and parsing {pkgname}")
    location <- paste0(templocation, "/",pkgname)
    try(download_tarfile(link, location))
    if(fs::file_exists(location)){
        untar_files_and_extract_info(location)
    }
}


check_local_packages <- function(){
    basename(as.character(fs::dir_ls(path =CRANEXTRACT)))
}
