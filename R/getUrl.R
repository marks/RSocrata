getUrl <- function(hostname, resourcePath, query){
    structure(list(scheme = "https",
                   hostname = getHostname(hostname),
                   port = NULL,
                   path = getResourcePath(resourcePath)$resourcePath,
                   query = getQuery(query),
                   params = NULL,
                   fragment = NULL,
                   username = NULL,
                   password = NULL),
              class = "url")
}
