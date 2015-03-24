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
#' 

read.socrata <- function(url = NULL,
                         hostname = NULL,
                         resourcePath = NULL,
                         query = NULL,
                         apptoken = NULL,
                         pagesize = 50000,
                         keyfield = NULL) {
    ##--------------------------------------------------------------------------
    ## Construct urlParsed from user input
    ##--------------------------------------------------------------------------
    ## If a URL is provided, then parse the URL
    ## Otherwise, validate and construct a parsed URL from the components
    if(!is.null(url)){
        urlParsed <- httr::parse_url(url)
        ## Standardize the path component
        urlParsed$path <- getResourcePath(urlParsed$path)
    } else {
        ## Validate and extract components, and build httr url object
        urlParsed <- getUrl(hostname, resourcePath, query)
    }
    ## Insert token into query part of url
    urlParsed$query <- insertToken(urlParsed$query, apptoken, urlParsed)
    ## Get the mimeType from the input
    mimeType <- getResourcePath(urlParsed$path)$mimeType
    ## Check url for minimum completeness
    validateUrl(urlParsed)
    
    ##--------------------------------------------------------------------------
    ## Set up for request pagnation
    ##--------------------------------------------------------------------------
    ## Get row count and calculate number of pages
    totalRows <- getQueryRowCount(urlParsed, mimeType)
    ## Calculate total requests    
    totalRequests <- trunc(totalRows / pagesize) + 1
    ## Get column names from 1 row of CSV download
    colNames <- getColumnNames(urlParsed, mimeType)
    ## Compose the base url to be used for requests, before adding pageing info
    urlBase <- httr::build_url(urlParsed)
    if(totalRequests > 1){
        ## Now reply on constructed limit argument, so remove any previous limit
        ## specification from query (it's interger(0) if no limit arg present)
        limitArg <- grep("\\$limit", names(urlParsed$query), ignore.case = TRUE)
        if(length(limitArg) > 0){
            urlParsed$query[[limitArg]] <- NULL
        }
        ## Get "urlFinal" which is actually several URLs that have the 
        ## $offset, $limit, and $order arguments embedded
        urlFinal <- getPagedQueries(urlBase, totalRows, pagesize, colNames, 
                                    keyfield)
    } else {
        urlFinal <- urlBase
    }
    resultRaw <- lapply(urlFinal, httr::GET)
    # saveRDS(resultRaw, "resultRaw.Rds")
    # resultRaw <- loadRDS("resultRaw.Rds")
    resultContent <- lapply(resultRaw, httr::content)
    resultContent <- do.call(c, resultContent)
    if(mimeType == "json"){
        resultContent <- unlistByName(resultContent)
    } else {
        resultContent <- do.call(rbind, resultContent)
    }
    columnDataTypes <- getColumnDataTypes(urlParsed)
    result <- data.frame(resultContent, stringsAsFactors = FALSE)
    for(j in 1:ncol(result)){
        # result[,j] <- type.convert(result[,j], as.is = TRUE)
        switch(columnDataTypes[j],
               number = {result[,j] <- as.numeric(result[,j])},
               calendar_date = {result[,j] <- posixify(result[,j])})
    }
    return(result)
    
    # response <- getResponse(validUrl)
    # page <- getContentAsDataFrame(response)
    # result <- page
    # dataTypes <- getSodaTypes(response)
    # ## More to come? Loop over pages implicitly
    # while (nrow(page) > 0) { 
    #     char <- if(is.null(urlParsed$query)) {'?'} else {"&"}
    #     query <- paste(validUrl, char, '$offset=', nrow(result), sep='')
    #     response <- getResponse(query)
    #     page <- getContentAsDataFrame(response)
    #     result <- rbind(result, page) # accumulate
    # }
    # # convert Socrata calendar dates to posix format
    # for(columnName in colnames(page)[
    #     !is.na(dataTypes[fieldName(colnames(page))]) & 
    #         dataTypes[fieldName(colnames(page))] == 'calendar_date']) {
    #     result[[columnName]] <- posixify(result[[columnName]])
    # }
    # result
}
