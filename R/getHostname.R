#' @name getHostname
#' @title Get Hostname
#' 
#' @param hostname        The hostname (and optionally scheme) part of the URL.
#' @param defaultScheme   The scheme if a valid scheme is not provided. 
#'                        Currently this value is \code{"https"}
#' @param validSchemes    Valid schemes, currently \code{c("http", "https")}
#' 
#' @return a list containing the hostname and scheme
#' @author Tom Schenk Jr \email{tom.schenk@@cityofchicago.org}
#' 
#' @description This function sets the hostname and scheme to be used.
#' 



# getHostname <- function(hostname, 
#                         defaultScheme = "https",
#                         validSchemes = c("http", "https")) {
#     
#     resultHostname <- httr::parse_url(hostname)$hostname
#     resultScheme <- httr::parse_url(hostname)$scheme
#     
#     ## Test validity of the scheme
#     if(!resultScheme %in% validSchemes){
#         resultScheme <- defaultScheme
#     }
#     
#     ## Return fourByFour and mimeType
#     result <- list(hostname = resultHostname,
#                    scheme = resultScheme)
#     return(result)
#     
# }

getHostname <- function(hostname) {
    
    resultHostname <- httr::parse_url(hostname)$hostname
    return(resultHostname)
    
}

