#' Convert Socrata calendar_date string to POSIX
#'
#' @param x character vector in one of two Socrata calendar_date formats
#' @return a POSIX date
#' @export
#' @author Hugh J. Devlin, Ph. D. \email{Hugh.Devlin@@cityofchicago.org}
posixify <- function(x) {
    x <- as.character(x)
    if (length(x)==0) return(x)
    # Two calendar date formats supplied by Socrata
    pattern_short <- "^[[:digit:]]{1,2}/[[:digit:]]{1,2}/[[:digit:]]{4}$"
    if(any(regexpr(pattern_short, x[1])[1] == 1)){
        ## short date format
        strptime(x, format="%m/%d/%Y") 
    } else{
        ## long date-time format
        strptime(x, format="%m/%d/%Y %I:%M:%S %p")
    }
}

