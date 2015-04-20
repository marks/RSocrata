getPagedQueries <- function(urlParsed, totalRows, pagesize, colNames, keyfield,
                            totalRequests){
    ## If the user has not specified a key field, use the first column, 
    ## which is most often an id (and it's the best guess for a key unless 
    ## Socrata enables the ability to query for a key / keys later on)
    if(is.null(keyfield)){
        warning("The 'keyfield' argument was not supplied to read.socrata, ",
                "using ", colNames[1], " to order results")
        keyfield <- colNames[1]
    }
    ## Likewise, if the user supplied a keyfield, but it's not found in the 
    ## column names, then use the first column name
    if(!is.null(keyfield) && !(keyfield %in% colNames) ){
        warning("The keyfield argument \"", keyfield, "\" was not found in ",
                "the column names of the returned data set.  Using \"", 
                colNames[1], "\" to order results instead")
        keyfield <- colNames[1]
    }
    
    ## Create the page request components
    qLimit <- sprintf("$limit=%.0f", pagesize)
    qOffset <- sprintf("$offset=%.0f", 
                       seq(from = 0, 
                           by = pagesize, 
                           length.out = totalRequests))
    qOrder <- paste0("$order=", keyfield)
    ## Create the base url for the requests
    if(is.null(urlParsed[["query"]])){
        urlBase <- paste0(httr::build_url(urlParsed), "?")
    } else {
        urlBase <- httr::build_url(urlParsed)
    }
    ## Assemble the page request query components with the urlParsed
    urlFinal <- paste(urlBase, qLimit, qOffset, qOrder, sep="&")
    ## Remove the "offset=0" from the first query
    urlFinal[1] <- gsub("\\&\\$offset=0", "", urlFinal[1])
    ## Make the last query only request the remaining rows
    urlFinal[[length(urlFinal)]] <- 
        gsub(pattern = sprintf("\\$limit=%.0f", 
                               pagesize), 
             replacement = sprintf("$limit=%.0f", 
                                   totalRows %% pagesize), 
             x = urlFinal[[length(urlFinal)]])
    
    ## Return urlFinal
    return(urlFinal)
}
