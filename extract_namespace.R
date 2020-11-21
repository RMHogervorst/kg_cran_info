# Extract imports and exports from NAMESPACE file.


#inputdata <- readLines("testdata/NAMESPACE_dplyr")
#expected_exported_dplyr <- getNamespaceExports("dplyr")


remove_quotations <- function(vec){
    stringr::str_replace_all(vec, "\"", "")
}
# remove_quotations('export("%.%" )') == "export(%.%)"

remove_all_comments <- function(inputdata){
    stringr::str_remove(inputdata,pattern = "#.*$")
}
#remove_all_comments(c("a<-1 # thiscango","#allthistoo","NULL ##")) == c("a<-1 ","","NULL ")
#
liquify <- function(filename) {
    readLines(filename,warn = FALSE) %>% 
        remove_all_comments() %>% 
        paste(collapse = " ") %>% 
        stringr::str_remove_all(pattern = "\\s+") %>%
        remove_quotations()
}

matching_string <- "A-z_%.0-9\\$,*<!+\\-"

extract_exported_functions <- function(inputdata){
    str_extract_all(inputdata, paste0("export\\([",matching_string, "]+\\)")) %>% 
        unlist() %>% 
        str_replace_all("export\\(","") %>%
        str_replace_all("\\)", "") %>% 
        strsplit(split=",") %>% 
        unlist()
}
# teststringexport <-"export(tbl_df)export(%.%)export(.datatable.aware)importFrom(utils,head)importFrom(utils,tail)useDynLib(dplyr)export(H5DSetDescriptor,h5mread)"
# expected_export = c("tbl_df","%.%", ".datatable.aware", "H5DSetDescriptor","h5mread")
# extract_exported_functions(teststringexport) == expected_export
# length(extract_exported_functions(liquify("testdata/NAMESPACE_dplyr"))) ==120

extract_exported_s3method <- function(inputdata){
    result <- str_extract_all(inputdata, paste0("S3method\\([",matching_string,"]+\\)")) %>% 
        unlist() %>% 
        str_replace_all("S3method\\(","") %>%
        str_replace_all("\\)", "")
    if(length(result)== 0){
        NULL
    }else{
        result
    }
}
# "S3method(as.tbl,tbl)" #don't care enough. leave it all in.
#extract_s3method("S3method(as.tbl,tbl,donkey)") 
#extract_s3method("S3method(table_fields,PostgreSQLConnection)")
# length(extract_exported_s3method(liquify("testdata/NAMESPACE_tibble")))==27
# length(extract_exported_s3method(liquify("testdata/NAMESPACE_dplyr")))==230


extract_exported_classes <- function(inputdata){
    str_extract_all(inputdata, paste0("exportClasses\\([",matching_string, "]+\\)")) %>% 
        unlist() %>% 
        str_replace_all("exportClasses\\(","") %>%
        str_replace_all("\\)", "") %>% 
        strsplit(split=",") %>% 
        unlist()
}
# {liquify("testdata/NAMESPACE_HDF5Array_bioconductor") %>% 
#     extract_exported_classes()} == c( 
#     "H5DSetDescriptor", "HDF5ArraySeed"  ,  "HDF5Array",            
#     "HDF5Matrix", "ReshapedHDF5ArraySeed", "ReshapedHDF5Array",    
#     "ReshapedHDF5Matrix",   "HDF5RealizationSink",  "TENxMatrixSeed",       
#     "TENxMatrix"   ,     "TENxRealizationSink" )
#length(extract_exported_classes(liquify("testdata/NAMESPACE_dplyr"))) ==1


extract_exported_methods <- function(inputdata){
    str_extract_all(inputdata, paste0("exportMethods\\([",matching_string,"]+\\)")) %>% 
        unlist() %>% 
        str_replace_all("exportMethods\\(","") %>%
        str_replace_all("\\)", "") %>% 
        strsplit(split=",") %>% 
        unlist()
}
# {liquify("testdata/NAMESPACE_HDF5Array_bioconductor") %>%
#     extract_exported_methods()} == "extractNonzeroDataByCol"
# "exportMethods(Arith,Compare,Logic,dim<-,dimnames,dimnames<-)" %>% extract_exported_methods()
# "exportMethods(Arith,!,Logic,dim<-,dimnames,dimnames<-,+,%*%)" %>% extract_exported_methods()
# {liquify("testdata/NAMESPACE_Matrix") %>%
#         extract_exported_methods()} %>% length(.) == 86

### imports 

extract_import_from <- function(inputdata){
    bare_matches <- 
        str_extract_all(inputdata, paste0("importFrom\\([",matching_string,"]+\\)")) %>% 
        unlist() %>% 
        str_replace_all("importFrom\\(","") %>%
        str_replace_all("\\)", "") %>% 
        str_split(",")
    if(length(bare_matches)>0 && all(lengths(bare_matches))==2 ){
        data.frame(
            package = purrr::map_chr(bare_matches, 1),
            fun = purrr::map_chr(bare_matches, 2)
        )    
    }
}
#nrow(liquify("testdata/NAMESPACE_dplyr") %>% extract_import_from())==4

# take first element of vec and make df
vec_df <- function(vec){
    packagename <- vec[[1]]
    rest <- vec[2:length(vec)]
    data.frame(
        stringsAsFactors = FALSE,
        package=packagename,
        fun = rest
    )
}

extract_import_classes_from <- function(inputdata){
    bare_matches <- 
        str_extract_all(inputdata, paste0("importClassesFrom\\([",matching_string,"]+\\)")) %>% 
        unlist() %>% 
        str_replace_all("importClassesFrom\\(","") %>%
        str_replace_all("\\)", "") %>% 
        str_split(",")
    if(length(bare_matches)>0 && min(lengths(bare_matches))>1){
        purrr::map_dfr(bare_matches, vec_df)
    }
}
extract_import_methods_from <- function(inputdata){
    bare_matches <- 
        str_extract_all(inputdata, paste0("importMethodsFrom\\([",matching_string,"]+\\)")) %>% 
        unlist() %>% 
        str_replace_all("importMethodsFrom\\(","") %>%
        str_replace_all("\\)", "") %>% 
        str_split(",")
    if(length(bare_matches)>0 && min(lengths(bare_matches))>1){
        purrr::map_dfr(bare_matches, vec_df)
    }
}

extract_import_all <- function(inputdata){
    bare_matches <- str_extract_all(inputdata, paste0("import\\([",matching_string,"]+\\)")) %>% 
        unlist() %>% 
        str_replace_all("import\\(","") %>%
        str_replace_all("\\)", "") %>% 
        str_split(",") %>% 
        unlist()
    if(length(bare_matches)>0){
        data.frame(
            stringsAsFactors = FALSE,
            package=bare_matches,
            fun = "*"
        )
    }
}

### end result
create_df_export <- function(namespace_text, fun, type){
    result <- fun(namespace_text)
    if (!is.null(result)) {
        data.frame(
            fun=result,
            type=type,
            stringsAsFactors = FALSE
        )
    }
}
create_df_import <- function(namespacetext, fun, type){
    result <- fun(namespacetext)
    if (!is.null(result)) {
        result$type <- type
        result
    }
}

all_exported_functions <- function(namespace_text){
    rbind(
        create_df_export(namespace_text,extract_exported_s3method,"S3"),
        create_df_export(namespace_text,extract_exported_functions,"regular"),
        create_df_export(namespace_text,extract_exported_methods,"S4 methods"),
        create_df_export(namespace_text,extract_exported_classes,"S4 classes")
    )    
}



#nrow(all_exported_functions("testdata/NAMESPACE_Matrix"))
#nrow(all_exported_functions("testdata/NAMESPACE_dplyr")) == 351


all_imported_functions <- function(namespace_text){
    rbind(
         create_df_import(namespace_text, extract_import_all, "entire_package"),
         create_df_import(namespace_text, extract_import_from, "import"),
         create_df_import(namespace_text, extract_import_classes_from, "S4 classes"),
         create_df_import(namespace_text, extract_import_methods_from, "S4 methods")
    )
}

#### function to write to the db
drop_pkg_namespace_into_db <- function(folder){
    name_ <- sub(pattern='CRAN/',replacement='',x=folder)
    file_loc <- paste0(folder,"/NAMESPACE")
    if(file.exists(file_loc)){
        if(runif(1) <.05){Sys.sleep(0.1)} # maybe this will help?
        log_info("Parsing namespace: {name_}")
        namespace_text <- liquify(file_loc)
        imports <- all_imported_functions(namespace_text)
        if(!is.null(imports)){
            imports$key <- name_
            log_debug("writing {nrow(imports)} rows to db for {name_}")
            dbAppendTable(con, name="imports",value = imports)    
        }
        exports <- all_exported_functions(namespace_text)
        if(!is.null(exports)){
            exports$key <- name_
            log_debug("writing {nrow(exports)} rows to db for {name_}")
            dbAppendTable(con, name="exports",value = exports)    
        }
        dbAppendTable(con, name="namespace_handled", 
                      value=data.frame(key=name_, namespace=1))
    }else{
        log_debug("Package {name_} has no NAMESPACE file")
        dbAppendTable(con, name="namespace_handled", 
                      value=data.frame(key=name_, namespace=0))
    }
}

retrieve_keys_imports_table <- function(){
        dbGetQuery(con, "SELECT DISTINCT key FROM namespace_handled;")$key
}

set_current_processed_to_namespace_handled_db <- function(){
    pkg_done <- unique(
        dbGetQuery(con, "SELECT DISTINCT key FROM imports;")$key,
        dbGetQuery(con, "SELECT DISTINCT key FROM exports;")$key        
    )
    dbAppendTable(con, name="namespace_handled", 
                  value=data.frame(key=pkg_done, namespace=1))
}