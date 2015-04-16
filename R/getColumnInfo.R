
getColumnInfo <- function(urlParsed){
    colUrl <- structure(list(scheme = urlParsed$scheme,
                             hostname = urlParsed$hostname,
                             path = paste0("views", 
                                           substr(urlParsed$path,9,18), 
                                           ".json")),
                        class = "url")
    ## Retreive all of the column data
    resultFull <- httr::content(httr::GET(httr::build_url(colUrl)))
    
    ## Extract just "name" and "fieldName" from the column data
    result <- list()
    result$name <- sapply(resultFull[["columns"]], 
                          function(x) x[["name"]])
    result$fieldName <- sapply(resultFull[["columns"]], 
                               function(x) x[["fieldName"]])
    result$renderTypeName <- sapply(resultFull[["columns"]], 
                                    function(x) x[["renderTypeName"]])
    return(result)
}

