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
                         keyfield = NULL,
                         useCluster = FALSE) {
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
    ## Rely on constructed limit argument, so remove any previous limit
    ## specification from query (it's interger(0) if no limit arg present)
    limitArg <- grep("\\$limit", names(urlParsed$query), ignore.case = TRUE)
    if(length(limitArg) > 0){
        urlParsed$query[[limitArg]] <- NULL
    }
    ## Get column names from 1 row of CSV download
    colNames <- getColumnNames(urlParsed, mimeType)
    ## Compose the base url to be used for requests, before adding pageing info
    urlBase <- httr::build_url(urlParsed)
    if(totalRequests > 1){
        ## Get "urlFinal" which is actually several URLs that have the 
        ## $offset, $limit, and $order arguments embedded
        urlFinal <- getPagedQueries(urlBase, totalRows, pagesize, colNames, 
                                    keyfield)
    } else {
        urlFinal <- urlBase
    }
    ## Download data, in parallel if requested
    if(useCluster){
        require(parallel)
        cl <- makeCluster(detectCores())
        resultRaw <- parLapply(cl, urlFinal, httr::GET)
        stopCluster(cl)
    } else {
        resultRaw <- lapply(urlFinal, httr::GET)
    }
    ## Temp code for loading intermediate results
    # saveRDS(resultRaw, "resultRaw.Rds")
    # resultRaw <- loadRDS("resultRaw.Rds")
    
    ## Extract content, and merge into one list
    resultContent <- lapply(resultRaw, httr::content)
    resultContent <- do.call(c, resultContent)
    ## Put results into matrix table form
    if(mimeType == "json"){
        resultContent <- unlistByName(resultContent)
    } else {
        resultContent <- do.call(rbind, resultContent)
    }
    result <- data.frame(resultContent, stringsAsFactors = FALSE)
    ## Reorder according to column names (put extra columns at end)
    colNamesMatched <- colnames(result)[colnames(result) %in% colNames]
    colNamesNotMatched <- colnames(result)[!colnames(result) %in% colNames]
    colNamesOrdered <- colNamesMatched[match(colNames, colNamesMatched)]
    colNamesOrdered <- colNamesOrdered[!is.na(colNamesOrdered)]
    colNamesResult <- c(colNamesOrdered, colNamesNotMatched)
    result <- result[, colNamesResult]
    
    ## Convert data types for result
    columnDataTypes <- getColumnDataTypes(urlParsed)
    numberColumns <- colNames[which(columnDataTypes == "number")]
    dateColumns <- colNames[which(columnDataTypes == "calendar_date")]
    for(j in numberColumns[numberColumns %in% colnames(result)]){
        result[,j] <- as.numeric(result[,j])
    }
    for(j in dateColumns[dateColumns %in% colnames(result)]){
        result[,j] <- as.POSIXct(result[,j])
    }
    return(result)
}

if(FALSE){
    ## Some test examples:
    rm(list=ls())
    geneorama::sourceDir("R/")
    
    url <- NULL
    hostname = "banannas://data.cityofchicago.org"
    ## Small number of records (1 request)
    #query = "?application_type=RENEW&license_description=Limited Business License&zip_code=60622"
    ## Big request (16 requests)
    # query = "?application_type=RENEW&license_description=Limited Business License"
    ## Testing the limit arg
    query = "?application_type=RENEW&license_description=Limited Business License&$limit=62000"
    resourcePath = "AnyRandomName/r5kz-chrr.json"
    apptoken = "bjp8KrRvAPtuf809u1UXnI0Z8"
    pagesize = 50000
    keyfield = "id"
    # keyfield = NULL
    useCluster
}
