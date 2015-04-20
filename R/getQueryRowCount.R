

getQueryRowCount <- function(urlParsed, mimeType){
    
    
    ## Construct a URL for getting the count of rows.
    ## Note that even if the query has a $limit argument, the count will be 
    ## based on the rest of the query so the $limit part of the query doesn't 
    ## need to be taken out.
    
    ## Get the query part of the URL, 
    if(is.null(urlParsed[['query']])){
        ## If there is no query at all, create a simple count
        cntQueryText <- "?$SELECT=COUNT(*)"
    } else {
        ## Otherwise, construct the query text with a COUNT command at the 
        ## beginning of any other limiting commands
        ## Reconstitute the httr url into a string
        cntQueryText <- httr::build_url(structure(urlParsed['query'], 
                                                  class = "url"))
        ## Add the COUNT command to the beginning of the query
        cntQueryText <- gsub(pattern = ".+\\?",
                             replacement = "?$SELECT=COUNT(*)&",
                             cntQueryText)
    }
    
    ## Combine the count query with the rest of the URL
    cntUrl <- paste0(urlParsed[[c('scheme')]], "://", 
                     urlParsed[[c('hostname')]], "/", 
                     urlParsed[[c('path')]], 
                     cntQueryText)
    ## Execute the query to count the rows
    totalRowsResult <- httr::GET(cntUrl)
    ## Parsing the result depends on the mime type
    if(mimeType=="json"){
        totalRows <- as.numeric(httr::content(totalRowsResult)[[1]]$count)
    } else {
        totalRows <- as.numeric(httr::content(totalRowsResult)$count)
    }
    
    ## Limit the row count to $limit (if the limit exists)
    queryLimitIndex <- grep(pattern = "\\$limit", 
                            x = names(urlParsed[['query']]), 
                            ignore.case = TRUE)
    if(!length(queryLimitIndex) == 0) {
        totalRows <- min(totalRows, 
                         as.numeric(urlParsed[["query"]][[queryLimitIndex]]))
    }
    return(totalRows)
}
