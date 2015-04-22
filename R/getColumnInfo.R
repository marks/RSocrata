
getColumnInfo <- function(urlParsed){
    colUrl <- structure(list(scheme = urlParsed$scheme,
                             hostname = urlParsed$hostname,
                             path = paste0("views", 
                                           substr(urlParsed$path,9,18), 
                                           ".json")),
                        class = "url")
    ## Retreive all of the column data
    resultFull <- httr::content(httr::GET(httr::build_url(colUrl)))
    
    ## Easy way to debug / view column data
    ## extract column data, then erase cachedContents
    # colinfo <- resultFull$columns
    # for(i in 1:length(colinfo)) colinfo[[i]]$cachedContents <- NULL
    # colinfo
    
    subcolumnExtractor <- function(colData, nameType){
        if("subColumnTypes" %in% names(colData)){
            paste(colData[[nameType]],
                  unlist(colData[["subColumnTypes"]]),
                  sep = ".")
        } else {
            colData[[nameType]]
        }
    }
    
    countSubcolumns <- function(colData){
        if("subColumnTypes" %in% names(colData)){
            length(unlist(colData[["subColumnTypes"]]))
        } else {
            1
        }
    }
    ## Extract just "name" and "fieldName" from the column data
    result <- list()
    result$name <- sapply(resultFull[["columns"]], 
                          function(x) x[["name"]])
    result$fieldName <- unlist(sapply(resultFull[["columns"]], 
                               subcolumnExtractor, nameType="fieldName"))
    result$renderTypeName <- sapply(resultFull[["columns"]], 
                                    function(x) x[["renderTypeName"]])
    
    
    subColumnCounts <- sapply(resultFull[["columns"]], countSubcolumns)
    result$renderTypeName_fieldName <- rep(result$renderTypeName, 
                                           subColumnCounts)
    
    return(result)
}

