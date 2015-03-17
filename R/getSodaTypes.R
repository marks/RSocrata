# Get the SoDA 2 data types
#
# Get the Socrata Open Data Application Program Interface data types from the http response header
# @author Hugh J. Devlin, Ph. D. \email{Hugh.Devlin@@cityofchicago.org}
# @param responseHeaders headers attribute from an httr response object
# @return a named vector mapping field names to data types
getSodaTypes <- function(response) { UseMethod('response') }
getSodaTypes <- function(response) {
    result <- fromJSON(response$headers[['x-soda2-types']])
    names(result) <- fromJSON(response$headers[['x-soda2-fields']])
    result
}

