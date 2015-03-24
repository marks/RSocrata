#' List datasets available from a Socrata domain
#'
#' @param url A Socrata URL. This simply points to the site root. 
#' @return an R data frame containing a listing of datasets along with
#' various metadata.
#' @export
#' @author Peter Schmiedeskamp \email{pschmied@@uw.edu}
#' @examples
#' df <- ls.socrata("http://soda.demo.socrata.com")
ls.socrata <- function(url) {
    url <- as.character(url)
    parsedUrl <- parse_url(url)
    if(is.null(parsedUrl$scheme) | is.null(parsedUrl$hostname))
        stop(url, " does not appear to be a valid URL.")
    parsedUrl$path <- "data.json"
    parsedUrl$query <- NULL
    df <- fromJSON(build_url(parsedUrl))
    df <- as.data.frame(df$dataset)
    df$issued <- as.POSIXct(df$issued)
    df$modified <- as.POSIXct(df$modified)
    df$theme <- as.character(df$theme)
    return(df)
}
