# Content parsers
#
# Return a data frame for csv
#
# @author Hugh J. Devlin \email{Hugh.Devlin@@cityofchicago.org}
# @param an httr response object
# @return data frame, possibly empty
getContentAsDataFrame <- function(response) { UseMethod('response') }
getContentAsDataFrame <- function(response) {
    mimeType <- response$header$'content-type'
    # skip optional parameters
    sep <- regexpr(';', mimeType)[1]
    if(sep != -1) mimeType <- substr(mimeType, 0, sep[1] - 1)
    switch(mimeType,
           'text/csv' = 
               content(response), # automatic parsing
           'application/json' = 
               if(content(response, as='text') == "[ ]") # empty json?
                   data.frame() # empty data frame
           else
               data.frame(t(sapply(content(response), unlist)), stringsAsFactors=FALSE)
    ) # end switch
}

