## Data for triangles in databases is typically stored
## in observational format. Various R packages for implementing Insurance
## triangles have defined classes for storing and displaying the data
## in triangle format
## Convert to a triangle This program provides functions for working with triangular data

library(ChainLadder)
library(MRMR)
library(plyr)
library(lubridate)
library(stringr)


## Start by reading in some sample data
x <- read.csv("data/sample_data.csv")
x$Origin <- as.Date(x$Origin, "%d/%m/%Y")


##data <- as.triangle(x, origin="Origin.label", dev="Development")

## Assuming the data has been read in quarterly, with date as origin and dev period as integer
## we convert it to Annual Quarterly
## Data could be either cumulative or incremental
## There is a function in the ChainLadder package that allows conversion
## convert quarterly data to annual Quarterly

## We need to be able to convert from incremental to cumulative and vice versa (Functions Provided in ChainLadder)
## A class is needed for storing triangular data (We use the triangle class provided in ChainLadder)
## We need to convert from lower level structures to Higher level e.g. AxQ and AxA. This is trivial if using data stored where the development part of the data is a number e.g. number of months
## Apply this function to the data stored in long format

## Return an Annual Quarterly triangle
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

printTri <- function(x,...) UseMethod("printTri")


# S3 Methods ----

#' Print triangle class objects
#' 
#' S3 method to control behaviour for printing triangle class objects
#' @param matrix Specify weather to print the data as an default R matrix \code{matrix=TRUE} or a nicely formatted triangle.
print.triangle <- function(x,matrix=FALSE,...) {
  if(matrix==TRUE) {
    print.default(x)
  }
  else {
    printTri(x, ...)
  }
}



#' Extract the details of the Triangle
#' 
#' Extract the class data from the triangle to use as
#' a heading for printing the data
#' @param tri triangle
#' 
getDetails <- function(tri){
  return(paste0(" ",class(tri)[1:length(class(tri))-1], collapse=" "))
}

#' Extract the Latest Cumulative Value
#' 
#' Extract last diagonal or the latest cumulative values
#' 
#' This is a trivial exercise for straightforward AxA matrices
#' We have introduced some complexity here which should enable
#' AxQ matrices to be analysed
#' @param tri triangle

getLatest <- function(tri, nrow) {
  f <- function(x) {
    tmp <- which(is.na(x))[1]-1
    if (is.na(tmp)) { 
      tmp <- length(x)
    }
    return(tmp)
  }
  
  diag.col <- apply(tri, 1, f)
  if (any(grepl("cumulative", class(tri)))) {
  return(sapply(1:nrow, function(x) tri[x,diag.col[x]]))
  }
  else {
    return(apply(tri, 1, sum, na.rm=T))
  }
}
#' Extract the Column Totals
#' 
#' Totals of each column can be useful to see with the data
#' 
#' @param tri triangle

getTotals <- function(tri) {
  
  totals <- apply(tri, 2, sum, na.rm=T)
  return(totals)
}



## dev.triangles printing function (nicely formatted triangles)
printTri.triangle <- function(x,last.rows=8L,first.cols=8L,last.cols=1L,first.rows=1L,...){
  stopifnot(length(last.rows) == 1L)
  stopifnot(length(first.cols) == 1L)
  stopifnot(length(last.cols) == 1L)
  stopifnot(length(first.rows) == 1L)
  
  ## Print the name of the matrix
  cat("\n")
  obj.nam <- getDetails(x)
  cat(paste(obj.nam,"\n"))
  cat(" ---------------------- \n\n")
  
  ## Get details of the matrix
  nrow <- nrow(x)
  ncol <- ncol(x)
  n.values <- nrow*ncol
  accident.periods <- dimnames(x)[[1]]
  development.periods <- dimnames(x)[[2]]
  
  
  ## We will only print at most 1 ... 8 accident years and the
  ## corresponding diagonal for amounts, for numbers we could print more
  ## Check that first.cols + last.cols > ncol
  ## Check that first.rows + last.rows > nrow
  ## Need to revisit this part
  
  if (first.cols + last.cols   > ncol) {
    first.cols = 1
    last.cols = ncol - first.cols
  }
  if (first.rows + last.rows   > nrow) {
    first.rows = 1
    last.rows = nrow - first.rows
  }

  #Round the numbers to 2 decimal places (for printing)
  x <- round(x, digits=0)
  
    
  ## Calculate the latest diagonal, or the final column
  latest.diag <- format(getLatest(x, nrow), big.mark=",", width=9)
  col.totals <- prettyNum(getTotals(x), big.mark=",", width=9)

  
  ## Convert the matrix to nicely formatted characters with minimum width of 9
  x <- format(x, big.mark=",", width=9)
  x <- gsub("NA","  ", x)
  

  
  ## Add headings
  ylabel <- "| Development\n"
  xlabel <- " Origin |\n"
  cat(format(ylabel, width=nchar(xlabel)+nchar(ylabel)-1, justify="right"))
  cat(xlabel)
  
  ## Add x axis labels (at the top)
  cat(format(" ", width=nchar(xlabel)-1))
  
  ## Print Development Period Headings
  for (i in 1:first.cols) {
    cat(format(development.periods[i], width=nchar(x[1]), justify="right"))
    cat(" ")
  }
  
#  if (ncol > 7) {
#    cat(format("   ",width=1))
#  }
  
  for (i in (ncol-last.cols+1):ncol) {
    cat(format(development.periods[i], width=nchar(x[1]), justify="right"))
    cat(" ")
  }
  cat("    ")
  cat (format("Latest", justify="right", width=nchar(x[1])))
  cat("\n")
  cat("\n")
  
  ## Add Triangle Data and row labels (y axis)
  for (i in 1:first.rows){
    cat(format(paste(accident.periods[i],"|"), width=nchar(xlabel)-1, justify="right"))
    for (j in 1:first.cols){
      cat(x[i+nrow*(j-1)])
      cat(" ")
    }
    if(nrow>7) {
      cat(" ---- ")
    }
    for (k in last.cols:1){
      cat(x[nrow*ncol-nrow*k+i])
      cat(" ")
    }
    
    cat("    ")
    cat(format(latest.diag[i], justify="right", width=10, big.mark=","))
    cat("\n")
  }
  
  sub.text <- paste0(format(" ", width=nchar(xlabel)+1),str_dup("-",nchar(x[1]))," ")
  if (nrow > 7) {
    for (j in 1:1){  
      cat(sub.text)
    }
    
    cat("\n")
    
    for (j in 1:1){
      cat(sub.text)
    }
    cat("\n")
  }
  for (i in (nrow-last.rows+1):nrow) { ##keep last n rows
    cat(format(paste(accident.periods[i],"|"), width=nchar(xlabel)-1, justify="right"))
    for (j in 1:first.cols) { 
      cat(x[i+nrow*(j-1)])
      cat(" ")
    }
    if (nrow > 7) {
      cat(" ---- ")
    }
    for (k in last.cols:1){
      cat(x[nrow*ncol-nrow*k+i])
      cat(" ")
    }
    cat("    ")
    cat(latest.diag[i])
    cat("\n")
  }
  
  cat("\n")
  cat(" ---------------------- \n\n")
  
  cat(format("Totals", width=nchar(xlabel)-1, justify="left"))
    for (j in 1:first.cols) { 
      cat(col.totals[j])
      cat(" ")
    }
  
    cat("\n\n")
  
}

