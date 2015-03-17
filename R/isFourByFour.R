#' Checks the validity of the syntax for a potential Socrata dataset Unique Identifier, also known as a 4x4.
#'
#' Will check the validity of a potential dataset unique identifier
#' supported by Socrata. It will provide an exception if the syntax
#' does not align to Socrata unique identifiers. It only checks for
#' the validity of the syntax, but does not check if it actually exists.
#' @param fourByFour a string; character vector of length one
#' @return TRUE if is valid Socrata unique identifier, FALSE otherwise
#' @author Tom Schenk Jr \email{tom.schenk@@cityofchicago.org}
isFourByFour <- function(fourByFour) {
    fourByFour <- as.character(fourByFour)
    if(nchar(fourByFour) != 9)
        return(FALSE)
    if(regexpr("[[:alnum:]]{4}-[[:alnum:]]{4}", fourByFour) == -1)
        return(FALSE)
    TRUE    
}

