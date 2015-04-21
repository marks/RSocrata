

##--------------------------------------------------------------------------
## A JSON TEST with uneven row lengths
##--------------------------------------------------------------------------
rm(list=ls())
gc(reset=T)
# install.packages('devtools')
# devtools::install_github("geneorama/geneorama")
geneorama::sourceDir("R/")

## Read in the data
system.time(
    data <- read.socrata(url = "https://data.cityofchicago.org/resource/kn9c-c2s2.json",
                         keyfield = "ca")
)



