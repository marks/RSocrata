## Extract data types from views
getColumnDataTypes <- function(urlParsed){
    colUrl <- structure(list(scheme = urlParsed$scheme,
                             hostname = urlParsed$hostname,
                             path = paste0("views", 
                                           substr(urlParsed$path,9,18), 
                                           ".json")),
                        class = "url")
    result <- httr::content(httr::GET(httr::build_url(colUrl)))
    ## Extract JSON names 
    result <- sapply(result[["columns"]], function(x) x[["renderTypeName"]])
    return(result)
}
