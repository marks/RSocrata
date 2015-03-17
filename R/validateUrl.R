#' Convert, if necessary, URL to valid REST API URL supported by Socrata.
#'
#' Will convert a human-readable URL to a valid REST API call
#' supported by Socrata. It will accept a valid API URL if provided
#' by users and will also convert a human-readable URL to a valid API
#' URL. Will accept queries with optional API token as a separate
#' argument or will also accept API token in the URL query. Will
#' resolve conflicting API token by deferring to original URL.
#' @param url  a string; character vector of length one
#' @param app_token a string; SODA API token used to query the data 
#' portal \url{http://dev.socrata.com/consumers/getting-started.html}
#' @return a valid Url
#' @author Tom Schenk Jr \email{tom.schenk@@cityofchicago.org}
validateUrl <- function(url, app_token) {
    url <- as.character(url)
    parsedUrl <- parse_url(url)
    if(any(sapply(parsedUrl[c('scheme', 'hostname', 'path')], is.null))){
        stop(url, " does not appear to be a valid URL.")
    }
    # Handle the addition of API token and resolve invalid uses:
    if(!is.null(app_token)) { 
        if(is.null(parsedUrl$query[["$$app_token"]])) {
            token_inclusion <- "valid_use"
        } else {
            token_inclusion <- "already_included" }
        switch(token_inclusion,
               "already_included"={ # Token already included in url argument
                   warning(url, paste0(" already contains an API token in url.",
                                       " Ignoring user-defined token."))
               },
               "valid_use"={ # app_token argument is used, not duplicative.
                   parsedUrl$query[["app_token"]] <- 
                       as.character(paste0("%24%24app_token=", app_token))
               })
    } 
    if(substr(parsedUrl$path, 1, 9) == 'resource/') {
        return(build_url(parsedUrl)) # resource url already
    }
    fourByFour <- basename(parsedUrl$path)
    if(!isFourByFour(fourByFour))
        stop(fourByFour, " is not a valid Socrata dataset unique identifier.")
    else {
        parsedUrl$path <- paste('resource/', fourByFour, '.csv', sep="")
        build_url(parsedUrl) 
    }
}

