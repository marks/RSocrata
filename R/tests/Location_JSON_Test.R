
##--------------------------------------------------------------------------
## JSON TEST WITH COLUMN SUBTYPES (IN LOCATION)
##--------------------------------------------------------------------------
rm(list=ls())
gc(reset=T)
# install.packages('devtools')
# devtools::install_github("geneorama/geneorama")
geneorama::sourceDir("R/")

## DEFINE EXAMPLES:
## Earthquake demos (note, they are http not https)
examp1 <- 'http://soda.demo.socrata.com/resource/4334-bgaj.json'
examp2 <- 'http://soda.demo.socrata.com/resource/4334-bgaj.csv'
## Building permit examples
examp3 <- "https://data.cityofchicago.org/resource/ydr8-5enu.json?$limit=100&$order=id"
examp4 <- "https://data.cityofchicago.org/resource/ydr8-5enu.json?$limit=100"
examp5 <- "https://data.cityofchicago.org/resource/ydr8-5enu.csv?$limit=100"
examp6 <- "https://data.cityofchicago.org/resource/ydr8-5enu.csv?$limit=100&$order=id"

## Read in the data
# system.time(
    dat1 <- read.socrata(url = examp1)
    dat2 <- read.socrata(url = examp2)
    
    dat3 <- read.socrata(url = examp3)
    dat4 <- read.socrata(url = examp4)
    dat5 <- read.socrata(url = examp5)
    dat6 <- read.socrata(url = examp6)
# )

## Check the structure
str(dat1)
str(dat2)
colnames(dat3)
colnames(dat4)
colnames(dat5)

dim(dat3)

head(dat1)
head(dat2)
head(dat3)
head(dat4)
head(dat5)


# ## Get the data.frame using fromJSON (Intersetingly, the columns are in the 
# ## right order here).  Notes also that fromJSON just creates a nested data.frame
# str(jsonlite::fromJSON(examp))
# str(jsonlite::fromJSON(examp2))
# ## That nested data.frame can be flattened with an jsonlite function:
# str(jsonlite::flatten(jsonlite::fromJSON(examp)))
# str(jsonlite::flatten(jsonlite::fromJSON(examp2)))
# 
# ## However, the names don't match the colInfo
# colInfo <- getColumnInfo(httr::parse_url(examp))
# colInfo
# 
# ## This is the same request using httr
# temp <- httr::content(httr::GET(examp))
# str(temp, 0)
# str(temp[[1]])
# names(unlist(temp[[1]]))
# 
# ## This is the same request using httr (CSV)
# temp2 <- httr::content(httr::GET(examp2))
# colnames(temp2)
# 
# 
# 
# ## Logic from "getColumnInfo"
# urlParsed <- httr::parse_url(examp3)
# ## Standardize the path component
# urlParsed$path <- getResourcePath(urlParsed$path)$resourcePath
# colUrl <- structure(list(scheme = urlParsed$scheme,
#                          hostname = urlParsed$hostname,
#                          path = paste0("views", 
#                                        substr(urlParsed$path,9,18), 
#                                        ".json")),
#                     class = "url")
# ## Retreive all of the column data
# resultFull <- httr::content(httr::GET(httr::build_url(colUrl)))
# 
# ## Easy way to debug / view column data
# ## extract column data, then erase cachedContents
# colinfo <- resultFull$columns
# for(i in 1:length(colinfo)) colinfo[[i]]$cachedContents <- NULL
# colinfo
# 
# subcolumnExtractor <- function(colData, nameType){
#     if("subColumnTypes" %in% names(colData)){
#         paste(colData[[nameType]],
#               unlist(colData[["subColumnTypes"]]),
#               sep = ".")
#     } else {
#         colData[[nameType]]
#     }
# }
# subcolumnExtractor(resultFull[["columns"]][[1]], "fieldName")
# subcolumnExtractor(resultFull[["columns"]][[131]], "fieldName")
# subcolumnExtractor(resultFull[["columns"]][[1]], "name")
# subcolumnExtractor(resultFull[["columns"]][[131]], "name")
# 
# subcolumnExtractor(resultFull[["columns"]], "fieldName", 131)
# 
# length(resultFull[["columns"]])
# 
# ## Extract just "name" and "fieldName" from the column data
# result <- list()
# result$name <- sapply(resultFull[["columns"]], 
#                       function(x) x[["name"]])
# result$name <- sapply(resultFull[["columns"]], 
#                       function(x) x[["name"]])
# result$fieldName <- sapply(resultFull[["columns"]], 
#                            function(x) x[["fieldName"]])
# result$renderTypeName <- sapply(resultFull[["columns"]], 
#                                 function(x) x[["renderTypeName"]])
# return(result)
# 
# i = 1
# i = 9
# if("subColumnTypes" %in% names(resultFull$columns[[i]])){
#     paste(resultFull$columns[[i]]$name, 
#           resultFull$columns[[i]]$subColumnTypes,
#           sep=".")
#     
# }
# resultFull$columns[[1]]$name
# resultFull$columns[[1]]$fieldName
# resultFull$columns[[1]]$renderTypeName
# resultFull$columns[[9]]$
# resultFull[["columns"]][["name"]]






