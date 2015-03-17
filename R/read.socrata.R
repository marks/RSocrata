#' Get a full Socrata data set as an R data frame
#'
#' Manages throttling and POSIX date-time conversions
#'
#' @param url A Socrata resource URL, 
#' or a Socrata "human-friendly" URL, 
#' or Socrata Open Data Application Program Interface (SODA) query 
#' requesting a comma-separated download format (.csv suffix), 
#' May include SoQL parameters, 
#' but is assumed to not include a SODA offset parameter
#' @param app_token a string; SODA API token used to query the data 
#' portal \url{http://dev.socrata.com/consumers/getting-started.html}
#' @return an R data frame with POSIX dates
#' @export
#' @author Hugh J. Devlin, Ph. D. \email{Hugh.Devlin@@cityofchicago.org}
#' @examples
#' df <- read.socrata("http://soda.demo.socrata.com/resource/4334-bgaj.csv")
read.socrata <- function(url, app_token = NULL) {
    validUrl <- validateUrl(url, app_token) # check url syntax, allow human-readable Socrata url
    parsedUrl <- parse_url(validUrl)
    mimeType <- guess_type(parsedUrl$path)
    if(!(mimeType %in% c('text/csv','application/json')))
        stop("Error in read.socrata: ", mimeType, " not a supported data format.")
    response <- getResponse(validUrl)
    page <- getContentAsDataFrame(response)
    result <- page
    dataTypes <- getSodaTypes(response)
    while (nrow(page) > 0) { # more to come maybe?
        query <- paste(validUrl, if(is.null(parsedUrl$query)) {'?'} else {"&"}, '$offset=', nrow(result), sep='')
        response <- getResponse(query)
        page <- getContentAsDataFrame(response)
        result <- rbind(result, page) # accumulate
    }    
    # convert Socrata calendar dates to posix format
    for(columnName in colnames(page)[!is.na(dataTypes[fieldName(colnames(page))]) & dataTypes[fieldName(colnames(page))] == 'calendar_date']) {
        result[[columnName]] <- posixify(result[[columnName]])
    }
    result
}

