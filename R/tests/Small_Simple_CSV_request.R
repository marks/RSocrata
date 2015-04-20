
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
                        resourcePath = "Administration-Finance/Current-Employee-Names-Salaries-and-Position-Title/xzkq-xp2w.csv",
                        query = NULL,
                        apptoken = NULL,
                        pagesize = 50000,
                        keyfield = NULL,
                        useCluster = FALSE)
)
str(dat)

system.time(
    dat <- read.socrata(url = NULL,
                        hostname = "httttp://data.cityofchicago.org",
                        resourcePath = "Administration-Finance/Current-Employee-Names-Salaries-and-Position-Title/xzkq-xp2w.csv",
                        query = "?$LimiT=100",
                        apptoken = NULL,
                        pagesize = 50000,
                        keyfield = NULL,
                        useCluster = FALSE)
)
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


