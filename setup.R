## Setup


create_folders <- function(){
    create_dir_if_not_exist(templocation)
    create_dir_if_not_exist(CRANEXTRACT)
    create_dir_if_not_exist("data")
}


create_dir_if_not_exist<- function(dirname){
    if(!fs::dir_exists(dirname)){
        fs::dir_create(path = dirname)
    }
}
