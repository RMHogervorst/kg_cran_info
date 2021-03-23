##### functions  -----
replace_null <- function(x){ifelse(is.null(x), NA_character_, x)}
fill_with_values <- function(listobject, lenghts){
    res <- rep(purrr::map_chr(listobject, paste, collapse=" "),lenghts)
    ifelse(res=="", NA_character_, res)
}
# extract all authors. author name, email role
extract_orchid <- function(comment){
    str_extract(comment, "[0-9]{4}-[0-9]{4}-[0-9]{4}-[0-9]{4}")
}
# one row per role
entity_role <- function(authors){
    # duplicate for every role
    roles_length <-  authors$role %>% lengths()
    res <- data.frame(
        given = fill_with_values(authors$given, roles_length),
        family= fill_with_values(authors$family, roles_length),
        email= fill_with_values(authors$email, roles_length),
        role= fill_with_values(authors$role, roles_length),
        comment=fill_with_values(authors$comment, roles_length)
    )
    res$ORCID <- extract_orchid(res$comment)
    res
}

make_email_adresses_correct<- function(string){
    string %>% 
    str_remove_all( "\\\\email") %>% 
    str_remove_all( "email =") %>% 
    str_replace_all(" ([+A-z0-9._-]+)\\@([A-z0-9.-]+) *"," <\\1@\\2> ") %>% 
    str_replace_all("[\\{\\(]{1}([+A-z0-9._-]+)\\@+([A-z0-9.-]+)[\\}\\)]{1}","<\\1@\\2> ")
}
# "Bailey Fosdick (bfosdick@uw.edu)"
# "Leontine Alkema (alkema@nus.edu.sg), Adrian Raftery (raftery@uw.edu)"
#  "Marcus G. Daniels mgd@swarm.org"
#  "Dr. Sanjay Bhatikar <sanjay.bhatikar@biobase.in>, Kanika Arora <kanika.arora@biobase." # dont
# "Valentin Todorov <valentin.todorov@chello.at>, Maria Anna Di Palma <madipalma@unior.it> and Michele Gallo \\email{mgallo@unior.it}"
fix_author_inconsistencies <- function(person){
    person %>% 
        make_email_adresses_correct() %>% 
        str_replace_all(";|&| and ",",") %>% # replace invalids & and ;
        str_replace_all(", and ", ", ") %>%  # oxford comma is killing me
        str_replace_all(">\\.", "> ") %>%  # replace invalid .
        str_replace_all(">,([A-z]+)", ">, \\1") %>%  # make sure there is a space after comma
        str_replace_all(">([A-z]+)", ">, \\1") %>%  # add a column if there are multiple
        str_replace_all("(>) +([A-z])", "\\1, \\2") %>% 
        str_replace_all(",,", ",") %>% 
        str_replace_all(", ,", ", ") %>% 
        str_replace_all(",([A-z]+)", ", \\1") %>% 
        str_replace_all(">, (\\[[a-z ]{3,}\\])", "> \\1")  # fix my own mistakes
}

place_comment <- function(given, comment, searchtext){
    idl <- str_detect(given, searchtext)
    comment[idl] <- searchtext
    comment
}

move_comments<- function(authors_df){
    authors_df$comment <- place_comment(authors_df$given, authors_df$comment, "S original by")
    authors_df$comment <- place_comment(authors_df$given, authors_df$comment, "R port by")
    authors_df$comment <- place_comment(authors_df$given, authors_df$comment, "with contributions by")
    authors_df$comment <- place_comment(authors_df$given, authors_df$comment, "with contributions from")
    authors_df$comment <- place_comment(authors_df$given, authors_df$comment, "[Aa]dditional contributions by")
    authors_df$comment <- place_comment(authors_df$given, authors_df$comment, "[Pp]ackaged for R by")
    authors_df$comment <- place_comment(authors_df$given, authors_df$comment, "[Bb]ased on earlier work by")
    authors_df$comment <- place_comment(authors_df$given, authors_df$comment, "with [A-z ]+ by|with [A-z ]+ from")
    # Based on earlier work by
    authors_df$given <- str_remove(authors_df$given, 
                                   "S original by|R port by|with contributions by|[Pp]ackaged for R by|with contributions from")
    authors_df$given <- str_remove(authors_df$given,"with [A-z ]+ by|with [A-z ]+ from|[Aa]dditional contributions by|[Bb]ased on earlier work by")
    authors_df
}

make_person <- function(person, role=NULL){
    if(is.null(role))stop("supply role in make_person",call. = FALSE)
    person <- fix_author_inconsistencies(person)
    personas <- as.person(person)
    if(length(personas)==1){
        if(is.null(personas$role)){personas$role <- role}
    }else if(max(lengths(personas$role))==0){
        personas$role <- role
    }
    personas
}

flatten_text <- function(start, end, pkg){
        paste(pkg[start:end],collapse = " ")
}
flatten_pkg_description <- function(pkg){
    pkg <- remove_all_comments(pkg)
    start_with_space <- str_detect(pkg, "^[\\s]")
    line_numbers <- 1:length(pkg)
    good_lines  <- line_numbers[!start_with_space]
    hold_frame <- data.frame(
        start_line = good_lines,
        end_line = good_lines
    )
    for (index in seq_along(good_lines)) {
        # if we are at the end take end of doc.
        if(index == length(good_lines)){
            hold_frame$end_line[index] <- length(line_numbers)
        }else if(good_lines[index]+1 == good_lines[index+1]){
            next
        }else{
            hold_frame$end_line[index] <- (good_lines[index+1] -1)
        }
    }
    
    purrr::map2_chr(
        hold_frame$start_line, 
        hold_frame$end_line,
        flatten_text, 
        pkg=pkg
    )
}

extract_from_line <- function(pkg, label){
    str_trim(str_replace(pkg[str_detect(pkg,paste0("^",label,":"))], paste0(label,":(.+$)"),"\\1"), "both")
}

extract_key <- function(pkg){
    package <- extract_from_line(pkg, "Package")
    version <- extract_from_line(pkg, "Version")
    paste0(package,"_", version)
}
fix_atR_inconsistencies <- function(text){
    # make email quoted
    text %>% 
    str_replace_all("[\'\"]{1}email[\'\"]*[ ]*= ", "email = ") %>% 
    str_replace_all("[ \'\"]([+A-z0-9._-]+)\\@([A-z0-9.-]+)[ \'\"]"," \"\\1@\\2\" ")
}
# c(person( \"Margaux Armfield\", \"email = margaux.armfield@gmail.com\", role = \"aut\"), person(\"Kennedy Dorsey\", email = \"kadorsey97@gmail.com\", role = c(\"aut\", \"cre\")))"

# parse authors@R
parse_authors <- function(flattened_pkg){
    atRperson <- as.person(NULL)
    atR <- extract_from_line(flattened_pkg,"Authors@R") %>% 
        str_replace_all("\\s", " ") %>% 
        str_replace_all("[ ]+", " ")
    if(length(atR) >0){
        atRperson <- eval(parse(text=atR %>% fix_atR_inconsistencies()))
    }
    ## dealing with authors@R that are placed in Author
    if(str_detect(extract_from_line(flattened_pkg,"Author"), "person\\(")){
        authors <- c(
            make_person(extract_from_line(flattened_pkg,"Maintainer"), role="cre"),
            extract_from_line(flattened_pkg,"Author") %>% 
                str_replace_all("\\s", " ") %>% 
                str_replace_all("[ ]+", " ") %>% 
                parse(text=.) %>% 
                eval()
        )
    }else{
        authors <- c(
            make_person(extract_from_line(flattened_pkg,"Author"),role = "aut"),
            make_person(extract_from_line(flattened_pkg,"Maintainer"), role="cre"),
            atRperson   
            )
    }
    authors_df <- entity_role(authors)
    authors_df <- move_comments(authors_df)
    authors_df[!duplicated(authors_df),]
}

dep_to_df <- function(string){
    res <- NULL
    if(length(string)>0){
        res <- data.frame(
            package_exact = stringr::str_split(string,",") %>% unlist()
        )
        res$version <- stringr::str_extract(res$package_exact,"\\(.+\\)") %>% str_remove("\\(") %>% str_remove("\\)")
        res$package <- stringr::str_remove(res$package_exact, "\\(.+\\)") %>% stringr::str_trim(side="both")
    }
    res[,c("package", "version", "package_exact")]
}

imports_ <- function(flattened_pkg){
    res <- dep_to_df(extract_from_line(flattened_pkg,"Imports"))
    if(!is.null(res)){
        res$type <- "Imports"
        res
    }
}

suggests_ <- function(flattened_pkg){
    res <- dep_to_df(extract_from_line(flattened_pkg,"Suggest"))
    if(!is.null(res)){
        res$type <- "Suggest"
        res
    }
}
depends_ <- function(flattened_pkg){
    res <- dep_to_df(extract_from_line(flattened_pkg,"Depends"))
    if(!is.null(res)){
        res$type <- "Depends"
        res
        }
}

#### dependencies
dependencies_dataframe <- function(flattened_pkg){
    rbind(
        imports_(flattened_pkg),
        suggests_(flattened_pkg),
        depends_(flattened_pkg)
    )
}

retrieve_keys_dependencies_description <- function(){
    dbGetQuery(con, "SELECT DISTINCT key FROM dependencies_description;")$key
}

dump_dependencies <- function(key, flattened_pkg){
    if(!key %in% retrieve_keys_dependencies_description()){
        dependencies <- dependencies_dataframe(flattened_pkg)
        if(!is.null(dependencies)){
            dependencies$key <- key
            log_debug("Adding {nrow(parsed_authors)} into dependencies_description table")
            dbAppendTable(con, "dependencies_description", value=dependencies)     
        }
    }else{
        log_info("package {key} already in dependencies_description table")
    }
}

s_dump_dependencies <- purrr::safely(dump_dependencies)


get_all_field_names<- function(flattened_pkg){
    str_remove(str_extract(flattened_pkg, "^.+?:"), ":")
}

nna_ <- function(res){
    replace_empty(res, variable=NA)
}
replace_empty <- function(res, variable){
    if(length(res)==0) res <- variable
    res
}

retrieve_keys_meta_table <- function(){
    dbGetQuery(con, "SELECT DISTINCT key FROM package_meta;")$key
}

#license, repository, packaged, needscompilation, date/publication, lazydata
dump_package_meta <- function(key, flattened_pkg){
    if(!key %in% retrieve_keys_meta_table()){
    res<- data.frame(
        packaged_datetime=nna_(extract_from_line(flattened_pkg, "Packaged")) %>% str_extract(".+;") %>% str_remove(";"),
        publication =nna_(extract_from_line(flattened_pkg, "Date/Publication")),
        license = nna_(extract_from_line(flattened_pkg, "License")),
        needscompilation = nna_(extract_from_line(flattened_pkg, "NeedsCompilation")),
        repository = replace_empty(extract_from_line(flattened_pkg, "Repository"),"CRAN"),
        lazydata = nna_(extract_from_line(flattened_pkg, "LazyData"))
    )
    res$key <- key
    dbAppendTable(con, "package_meta", value=res)  
    }else{
        log_info("package {key} already in package_meta table")
    }
    
}

s_dump_package_meta <- purrr::safely(dump_package_meta)
    


dump_package_other<- function(key, flattened_pkg, fields_left){
    if(!key %in% (dbGetQuery(con, "SELECT DISTINCT key FROM package_other;")$key) ){
        if(length(unique(fields_left)) != length(fields_left)){log_warn("Double field names in DESCRIPTION of {key}")}
            res <- data.frame(
                key= key,
                field = unique(fields_left)
            )
        if(nrow(res)>0){
            res$value <- purrr::map_chr(res$field, ~nna_(paste(extract_from_line(flattened_pkg, .x),collapse = ", ")))
            log_debug("Adding {nrow(res)} rows to package other")
            dbAppendTable(con, "package_other", value=res)
        }
    }else{
        log_info("package {key} already in package_other table")
    }
}
s_dump_package_other <- purrr:::safely(dump_package_other)

remove_empty_line <- function(flattened_pkg){
    flattened_pkg[flattened_pkg != ""]
}

# authors  #### 


retrieve_keys_authors_table <- function(){
    dbGetQuery(con, "SELECT DISTINCT key FROM authors;")$key
}

dump_authors <- function(key, flattened_pkg){
    if(!key %in% retrieve_keys_authors_table()){
        parsed_authors <- parse_authors(flattened_pkg)
        parsed_authors$key<-key
        log_debug("Adding {nrow(parsed_authors)} into authors table")
        dbAppendTable(con, "authors", value=parsed_authors)    
    }else{
        log_info("package {key} already in authors table")
    }
    
}

s_dump_authors <- purrr::safely(dump_authors)

parsed_fields <- c("Package", "Version", "Author", "Maintainer", 
                   "Authors@R", "Imports", "Suggests", "Depends",
                   "NeedsCompilation", "License","Date/Publication",
                   "Packaged", "Repository", "Type", "LazyData")
DESCRIPTION_all_done<- function(){
    base::intersect(
         base::intersect(
             retrieve_keys_meta_table(),   
             retrieve_keys_authors_table()
         ),
          base::intersect(
             retrieve_keys_dependencies_description(),
             (dbGetQuery(con, "SELECT DISTINCT key FROM package_other;")$key)
         )
    )
}


##### Extract Description into db   ----
parse_description_file <- function(package_folder){
    if(!fs::file_exists(paste0(package_folder,"/DESCRIPTION"))){
        log_warn("WARNING: no DESCRIPTION file in folder{package_folder}")
    }else{
        flattened_pkg <- 
            readLines(paste0(package_folder,"/DESCRIPTION"),warn = FALSE) %>% 
            flatten_pkg_description() %>% 
            str_replace_all("\\s", " ") %>% 
            str_replace_all("  ", " ") %>% 
            remove_empty_line()
        all_field_names <- get_all_field_names(flattened_pkg)
        fields_left <- all_field_names[!all_field_names %in% parsed_fields]
        key <- extract_key(flattened_pkg) 
        ## db actions
        s_dump_authors(key, flattened_pkg)
        s_dump_dependencies(key, flattened_pkg)
        s_dump_package_meta(key, flattened_pkg)
        s_dump_package_other(key, flattened_pkg, fields_left)
        if(runif(1) <0.001){Sys.sleep(0.1)} # give the machine some time to catch up
    }
}
