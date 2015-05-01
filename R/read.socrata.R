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
        urlParsed$path <- getResourcePath(urlParsed$path)$resourcePath
    } else {
        ## Validate and extract components, and build httr url object
        urlParsed <- getUrl(hostname, resourcePath, query)
    }
    ## Sanatize query part of parsed url
    queryResults <- validateUrlQuery(urlParsed,
                                     apptoken,
                                     keyfield)
    urlParsed$query <- queryResults$query
    rowLimit <- queryResults$rowLimit
    keyfield <- queryResults$keyfield
    
    ## Get the mimeType from the input
    mimeType <- getResourcePath(urlParsed$path)$mimeType
    ## Check url for minimum completeness
    validateUrl(urlParsed)
    
    ##--------------------------------------------------------------------------
    ## Set up for request pagnation
    ##--------------------------------------------------------------------------
    ## Get row count and calculate number of pages
    totalRows <- getQueryRowCount(urlParsed, mimeType, rowLimit)
    ## Calculate total requests
    totalRequests <- trunc(totalRows / pagesize) + 1
    ## Get column names from the views resource path
    colInfo <- getColumnInfo(urlParsed)
    ## Get "urlFinal" which is actually several URLs that have the 
    ## $offset, $limit, and $order arguments embedded
    urlFinal <- getPagedQueries(urlParsed, totalRows, pagesize, 
                                colInfo$fieldName, keyfield, totalRequests)
    ##------------------------------------------------------------------------
    ## Download data, in parallel if requested
    ##------------------------------------------------------------------------
    if(useCluster){
        require(parallel)
        cl <- makeCluster(detectCores())
        resultRaw <- parLapply(cl, urlFinal, httr::GET)
        # stopCluster(cl)
    } else {
        ## The subsequent code is equivalent to this:
        # resultRaw <- lapply(urlFinal, httr::GET)
        
        ## Get raw results in more detail, with timings:
        resultRaw <- list()
        resultRawTimingDetail <- list()
        for(u in urlFinal){
            cat("httr::GET call for request ", which(u==urlFinal), " of ", 
                totalRequests, "\n")
            print(u)
            resultRawTimingDetail[[u]] <- system.time(
                resultRaw[[u]] <- httr::GET(u)
            )
            print(resultRawTimingDetail[[u]])
        }
    }
    
    ## Extract content
    if(useCluster){
        # require(parallel)
        # cl <- makeCluster(detectCores())
        resultContent <- parLapply(cl, resultRaw, httr::content)
        stopCluster(cl)
    } else {
        resultContent <- list()
        for(i in 1:length(resultRaw)){
            cat("httr::content call for request ", i, " of ", totalRequests, "\n")
            #resultRaw[[i]] <- httr::content(resultRaw[[i]])
            print(system.time(
                resultContent[[i]] <- httr::content(resultRaw[[i]])
            ))
        }
    }
    
    ## Combine content
    if(mimeType == "json"){
        ## Merge extracted content
        resultContent <- do.call(c, resultContent)
        ## Put results into matrix table form
        resultContent <- unlistByName(resultContent, colInfo, mimeType)
    } else {
        resultContent <- do.call(rbind, resultContent)
    }
    result <- data.frame(resultContent, stringsAsFactors = FALSE)
    
    ## Convert data types for result
    if(mimeType == "json"){
        numberColumns <- which(colInfo$renderTypeName_fieldName == "number")
        dateColumns <- which(colInfo$renderTypeName_fieldName == "calendar_date")
    } else {
        numberColumns <- which(colInfo$renderTypeName == "number")
        dateColumns <- which(colInfo$renderTypeName == "calendar_date")
    }
    for(j in numberColumns){
        cat("Converting ", which(j==numberColumns), "th column of ", 
            length(numberColumns), " numeric columns\n")
        result[ , j] <- as.numeric(result[ , j])
    }
    for(j in dateColumns){
        cat("Converting ", which(j==dateColumns), "th column of ", 
            length(dateColumns), " date columns\n")
        if(mimeType=="json"){
            result[ , j] <- as.POSIXct(result[ , j])
        } else {
            result[ , j] <- as.POSIXct(strptime(result[ , j], "%m/%d/%Y"))
        }
    }
    return(result)
}

