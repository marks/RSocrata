#' @name getFourByFour
#' @title Get 4x4
#' 
#' @param resourcePath    A string containing the Socrata 4x4 and data type
#' @param defaultDataType The data type if a valid data type is not provided. 
#'                        Currently this value is \code{"csv"}
#' @param validDataTypes  Types of data allowed in request, defaults to 
#'                        \code{c("csv", "json")}
#' @return a list containing the Socrata 4x4 and the requested data type
#' @author Tom Schenk Jr \email{tom.schenk@@cityofchicago.org}
#' 
#' @description Extracts a Socrata dataset Unique Identifier, also known as a 
#'              4x4, and parses the data type requested by the user, if it 
#'              exists.  If the user has not specifiied a data type, the 
#'              default is returned.
#' @details     Currently the data type must be one of "csv" or "json", this 
#'              is a paramaterized value, but those paramaters are not 
#'              available to the user as this function is not exported, and
#'              no arguments are passed along to this function.
#'              
#'              Valid resourcePath examples include:
#'                  "4334-bgaj"
#'                  "resource\4334-bgaj"
#'                  "The-Actual-Resource-Name\4334-bgaj"
#'                  "Complete-Garbage\4334-bgaj"
#'              Also, any combination of json or csv may be included
#'                  "4334-bgaj.csv"
#'                  "resource\4334-bgaj"
#'                  "The-Actual-Resource-Name\4334-bgaj.json"
#'                  "Complete-Garbage\4334-bgaj.json"
#'              In the event that the trailing .[datatype] is invlalid, csv is 
#'              used instead
#'                  "4334-bgaj.csv_xjson" would be returned as "csv"
#'              
#' 
 
getResourcePath <- function(resourcePath, 
                            defaultDataType = "csv",
                            validDataTypes = c("csv", "json")) {
    ## Extract 4x4
    fourByFour <- substr(basename(as.character(resourcePath)), 1, 9)
    ## Test validity of fourByFour
    if(!isFourByFour(fourByFour)){
        stop(paste(fourByFour, "is not a valid Socrata 4x4"))
    }
    ## Extract mime type
    mimeType <- gsub(".+\\.", "", resourcePath)
    ## Test validity of mimeType
    if(!mimeType %in% validDataTypes){
        mimeType <- defaultDataType
    }
    ## Construct valid resource path
    result <- file.path("resource", paste0(fourByFour, ".", mimeType))
    return(list(resourcePath = result,
                fourByFour = fourByFour,
                mimeType = mimeType))
    
}

