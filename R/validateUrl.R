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
validateUrl <- function(parsedUrl) {
    if(any(sapply(parsedUrl[c('scheme', 'hostname', 'path')], is.null))){
        stop(url, " does not appear to be a valid URL.")
    }
    invisible(TRUE)
}

