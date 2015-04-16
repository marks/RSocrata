


# unlistByName <- function(dat) {
#     ## Use this example record to create a list of "master names"
#     MasterNames <- unique(names(unlist(dat, recursive = T)))
#     MasterNames
#     
#     ## Make a data object to match the dimension of the output
#     result <- matrix(data = NA, 
#                      nrow = length(dat), 
#                      ncol=length(MasterNames))
#     colnames(result) <- MasterNames
#         
#     ## Unlist each element
#     for(i in 1:length(dat)){
#         # if(i%%10000 == 0) print(i)
#         result[i, ] <- unname(unlist(dat[[i]], recursive = TRUE)[MasterNames])
#     }
#     
#     return(result)
# }

unlistByName <- function(dat, colInfo, mimeType) {
    ## Use this example record to create a list of "master names"
    MasterNames <- switch(mimeType, 
                          json = colInfo$fieldName,
                          csv = colInfo$name)
    
    ## Make a data object to match the dimension of the output
    result <- matrix(data = NA, 
                     nrow = length(dat), 
                     ncol=length(MasterNames))
    colnames(result) <- MasterNames
    
    ## Unlist each element
    for(i in 1:length(dat)){
        # if(i%%10000 == 0) print(i)
        result[i, ] <- unname(unlist(dat[[i]], recursive = TRUE)[MasterNames])
    }
    
    return(result)
}


