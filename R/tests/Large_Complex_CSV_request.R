
##--------------------------------------------------------------------------
## A COMPLEX JSON TEST
##--------------------------------------------------------------------------
rm(list=ls())
gc(reset=T)
# install.packages('devtools')
# devtools::install_github("geneorama/geneorama")
geneorama::sourceDir("R/")

## Start the profiler
# Rprof("socrataprofile.out", line.profiling=TRUE)

## Read in the data
system.time(
    dat <- read.socrata(url = NULL,
                        hostname = "httttp://data.cityofchicago.org",
                        resourcePath = "AnyRandomName/r5kz-chrr.csv",
                        query = "?application_type=RENEW&license_description=Limited Business License",
                        apptoken = "bjp8KrRvAPtuf809u1UXnI0Z8",
                        pagesize = 50000,
                        keyfield = "id",
                        useCluster = FALSE)
)
## Check the structure
str(dat)

## Turn off the profiler
# Rprof(NULL)

## View the profiler results
# summaryRprof("socrataprofile.out", lines = "show")

## Remove profile code
# file.remove("socrataprofile.out")


## View memory usage
gc(reset=TRUE)
geneorama::lll()
memory.profile()


