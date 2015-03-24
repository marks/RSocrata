

insertToken <- function(query, 
                        apptoken = NULL, 
                        url) {
    
    if(!is.null(apptoken)){
        ##----------------------------------------------------------------------
        ## Handle the insertion of the API token into the query, if it exists
        ##----------------------------------------------------------------------
        ## Convert to character, just in case
        apptoken <- as.character(apptoken)
        if(is.null(query["$$app_token"][[1]]) && !is.null(apptoken)) { 
            ## The user supplied an apptoken only through apptoken, so add
            ## apptoken to the query
            # query[['app_token']] <- as.character(paste0("%24%24app_token=", apptoken))
            query[["$$app_token"]] <- apptoken
        } else {
            ## The user supplied apptoken, there was already one in the query 
            ## and they don't match.
            if(query[["$$app_token"]] != apptoken){
                msg <- paste0("url = ", httr::build_url(url), "\n",
                              "The supplied url has an API token embedded in ",
                              "it, and the embedded token (",
                              query[['$$app_token']], ") does not match the ",
                              "supplied apptoken (", apptoken ,").\n",
                              "Using apptoken.")
                warning(msg)
                ## query[['$$app_token']] <- as.character(paste0("%24%24app_token=", apptoken))
                query[["$$app_token"]] <- apptoken
            }
        }
    }
    return(query)
}
