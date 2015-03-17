# Wrap httr GET in some diagnostics
# 
# In case of failure, report error details from Socrata
# 
# @param url Socrata Open Data Application Program Interface (SODA) query
# @return httr response object
# @author Hugh J. Devlin, Ph. D. \email{Hugh.Devlin@@cityofchicago.org}
getResponse <- function(url) {
    response <- GET(url)
    status <- http_status(response)
    if(response$status_code != 200) {
        msg <- paste("Error in httr GET:", response$status_code, 
                     response$headers$statusmessage, url)
        if(!is.null(response$headers$`content-length`) && 
               (response$headers$`content-length` > 0)) {
            details <- content(response)
            msg <- paste(msg, details$code[1], details$message[1])
        }
        logMsg(msg)
    }
    stop_for_status(response)
    response
}

