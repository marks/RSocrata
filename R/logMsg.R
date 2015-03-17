#' Time-stamped message
#'
#' Issue a time-stamped, origin-stamped log message. 
#' @param s a string
#' @return None (invisible NULL) as per cat
#' @author Hugh J. Devlin \email{Hugh.Devlin@@cityofchicago.org}
logMsg <- function(s) {
    cat(format(Sys.time(), "%Y-%m-%d %H:%M:%OS3 "), as.character(sys.call(-1))[1], ": ", s, '\n', sep='')
}

