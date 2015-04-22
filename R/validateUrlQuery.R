validateUrlQuery <- function(urlParsed,
                             apptoken,
                             keyfield) {
    
    ## Insert token into query part of url
    urlParsed[["query"]] <- insertToken(urlParsed = urlParsed,
                                        apptoken = apptoken)
    
    ## Extract the key field (aka order) from the parsedUrl, if it exists
    ## And remove $order from query
    orderArg <- grep(pattern = "\\$order", 
                     x = names(urlParsed[["query"]]), 
                     ignore.case = TRUE)
    if(length(orderArg) > 0){
        if(is.null(keyfield)){
            ## Use the order argument for keyfield
            keyfield <- urlParsed[["query"]][[orderArg]]
        } else {
            if(keyfield != urlParsed[["query"]][[orderArg]]) {
                warning(paste0("The keyfield provided does not match the keyfield ",
                               "implied by the URL argument / arguments\n",
                               "KEYFIELD SUPPLIED: ", keyfield, "\n",
                               "ORDER ARGUMENT IN URL: ", 
                               urlParsed[["query"]][[orderArg]], "\n",
                               "Defaulting to ", keyfield))
            }
        }
        ## remove $order from parsed url
        urlParsed[["query"]][[orderArg]] <- NULL
    }

    ## Rely on constructed limit argument, so remove any previous limit
    ## specification from query (it's interger(0) if no limit arg present)
    limitArg <- grep(pattern = "\\$limit", 
                     x = names(urlParsed[["query"]]), 
                     ignore.case = TRUE)
    if(length(limitArg) > 0){
        rowLimit <- urlParsed[["query"]][[limitArg]]
        urlParsed[["query"]][[limitArg]] <- NULL
    } else {
        rowLimit <- NULL
    }
    
    ## If there is nothing left in the query then remove it
    if(length(urlParsed[["query"]]) == 0){
        urlParsed[["query"]] <- NULL
    }
    
    return(list(query = urlParsed$query,
                keyfield = keyfield,
                rowLimit = rowLimit))
}
