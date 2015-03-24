


unlistByName <- function(dat, convertTypes=TRUE) {
    ## Use this example record to create a list of "master names"
    MasterNames <- unique(names(unlist(dat, recursive = T)))
    MasterNames
    
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
    
    if(convertTypes){
        ## Note, as.is == TRUE to avoid factor conversions
        result <- data.frame(apply(result, 2, type.convert, as.is=TRUE), 
                             stringsAsFactors=FALSE)
    }
    return(result)
}




