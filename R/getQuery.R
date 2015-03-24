


getQuery <- function(query){
    if(is.list(query)){
        result <- query
    } else {
        if(substr(query, 1,1)!="?") {
            query <- paste0("?", query)
        }
        result <- httr::parse_url(query)$query
    }
    return(result)
}
