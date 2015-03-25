
## Original simple version
# getColumnNames <- function(urlParsed){
#     
#     colUrl <- structure(list(scheme = urlParsed$scheme,
#                              hostname = urlParsed$hostname,
#                              path = paste0(substr(urlParsed$path,1,18), ".csv"),
#                              query = "$limit=1"),
#                         class = "url")
#     result <- colnames(httr::content(httr::GET(httr::build_url(colUrl))))
#     return(result)
# }

## Second version that pulls json and csv names
getColumnNames <- function(urlParsed, mimeType){
    colUrl <- structure(list(scheme = urlParsed$scheme,
                             hostname = urlParsed$hostname,
                             path = paste0("views", 
                                           substr(urlParsed$path,9,18), 
                                           ".json")),
                        class = "url")
    result <- httr::content(httr::GET(httr::build_url(colUrl)))
    if(mimeType=="json"){
        ## Extract JSON names 
        result <- sapply(result[["columns"]], function(x) x[["fieldName"]])
    } else {
        ## Extract CSV names
        result <- sapply(result[["columns"]], function(x) x[["name"]])
    }
    return(result)
}

