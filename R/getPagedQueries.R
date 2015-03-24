getPagedQueries <- function(urlBase, totalRows, pagesize, colNames, keyfield){
    ## If the user has not specified a key field, use the first column, 
    ## which is most often an id (and it's the best guess for a key unless 
    ## Socrata enables the ability to query for a key / keys later on)
    if(is.null(keyfield)){
        warning("The 'keyname' arg was not supplied, using ", colNames[1],
                " to order results")
        keyfield <- colNames[1]
    }
    ## Likewise, if the user supplied a keyfield, but it's not found in the 
    ## column names, then use the first column name
    if(!is.null(keyfield) && !(keyfield %in% colNames) ){
        warning("The 'keyname' arg was not found in the column names of ",
                "the returned data set.  Using ", colNames[1],
                " to order results instead")
        keyfield <- colNames[1]
    }        ## Assemble the page request portion of the queryies
    qLimit <- sprintf("$limit=%g", pagesize)
    qOffset <- sprintf("$offset=%g", 
                       seq(from = 0, 
                           by = pagesize, 
                           length.out = totalRequests))
    qOrder <- paste0("$order=", keyfield)
    urlFinal <- paste(urlBase, qLimit, qOffset, qOrder, sep="&")
    ## Remove the "offset=0" from the first query
    urlFinal[1] <- gsub("\\&\\$offset=0", "", urlFinal[1])
    ## Make the last query only request the remaining rows
    urlFinal[[length(urlFinal)]] <- 
        gsub(pattern = sprintf("\\$limit=%g", 
                               pagesize), 
             replacement = sprintf("$limit=%g", 
                                   totalRows %% pagesize), 
             x = urlFinal[[length(urlFinal)]])
    
    ## Return urlFinal
    return(urlFinal)
    
}