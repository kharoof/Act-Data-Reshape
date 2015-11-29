## Data for triangles in databases is typically stored
## in observational format. Various R packages for implementing Insurance
## triangles have defined classes for storing and displaying the data
## in triangle format
## Convert to a triangle

library(ChainLadder)
library(MRMR)
library(plyr)
library(lubridate)


## Start by reading in some sample data
x <- read.csv("data/sample_data.csv")
x$Origin <- as.Date(x$Origin, "%d/%m/%Y")


##data <- as.triangle(x, origin="Origin.label", dev="Development")

## Assuming the data has been read in quarterly,
## we convert it to Annual Quarterly
## Data could be either cumulative or incremental
## There is a function in the ChainLadder package that allows conversion
## convert quarterly data to annual Quarterly
AxQ <- function(x, ...) {
    ##Add some pivot fields
    x$Origin.label <- paste0(year(x$Origin), " Q", quarter(x$Origin))
    x$Development.Month <- x$Development + quarter(x$Origin)*3 -3
    x$Origin.Year <- year(x$Origin)
    #Pivot the data
    data.AxQ <- ddply(x, .(Origin.Year, Development.Month), summarize, value = sum(value))
    #Return a triangle
    as.triangle(data.AxQ, origin="Origin.Year", dev="Development.Month")
}
