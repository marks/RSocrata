

insertToken <- function(query, 
                        apptoken = NULL, 
                        urlParsed) {
    
    if(!is.null(apptoken)){
        ## If there is no query at all, create a base
        if(is.null(query)){
            query  <- list()
        }
        ## Convert to character, just in case
        apptoken <- as.character(apptoken)
        ## If the user supplied apptoken as an argument, and there was already 
        ## one in the query, and they don't match, then warn
        queryHasAppToken <- !is.null(query["$$app_token"][[1]])
        if(queryHasAppToken && query[["$$app_token"]] != apptoken){
            msg <- paste0("url = ", httr::build_url(urlParsed), "\n",
                          "The supplied url has an API token embedded in ",
                          "it, and the embedded token (",
                          query[['$$app_token']], ") does not match the ",
                          "supplied apptoken (", apptoken ,").\n",
                          "Using apptoken.")
            warning(msg)
        }
        
        ## Insert apptoken into the query (even if it already exists)
        query[["$$app_token"]] <- apptoken
    }
    return(query)
}
