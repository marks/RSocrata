

getQueryRowCount <- function(parsedUrl, mimeType){
    if(is.null(parsedUrl[["query"]][["$limit"]])){
        ## If the parsed URL's query does not have a $limit then count the rows
        ## based on the rest of the query
        cntQueryText <- httr::build_url(structure(parsedUrl['query'], 
                                                    class = "url"))
        cntQueryText <- gsub(pattern = ".+\\?",
                               replacement = "?$SELECT=COUNT(*)&",
                               x = cntQueryText)
        cntUrl <- paste0(parsedUrl[[c('scheme')]], "://", 
                          parsedUrl[[c('hostname')]], "/", 
                          parsedUrl[[c('path')]], 
                          cntQueryText)
        totalRowsResult <- httr::GET(cntUrl)
        if(mimeType=="json"){
            totalRows <- as.numeric(httr::content(totalRowsResult)[[1]]$count)
        } else {
            totalRows <- as.numeric(httr::content(totalRowsResult)$count)
        }
    } else {
        ## If the parsed URL's query has a $limit, then use $limit
        totalRows <- as.numeric(parsedUrl[["query"]][["$limit"]])
    }
    return(totalRows)
}
