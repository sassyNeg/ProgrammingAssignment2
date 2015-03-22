## ##################################################################################
## The functions in this file do the following:
##
## Function:  makeCacheMatrix
## 1. Save a matrix to cache using lexical scoping
## 2. Get a matrix from cache
## 3. Take a numeric inverted matrix and save it to cache
## 4. Retrieve the stored inverted matrix if it exists in cache 
##
##  Function:  cacheSolve  
## 1. Get chached matrix from cache if it exists
## 2. If matrix doesn't exist in cache, invert the matrix and call the setCachedmatrix 
##    function to save it to cache
## #################################################################################

## Function that has functions within it that store a matrix in cache 
## and retrieves it from cache
makeCacheMatrix <- function(x = matrix()) 
{

    ##Initialize matrix cache object
    matrixCached <- NULL
  
    ## This function retrieves a matrix
    get <- function () x
  
    ## This function saves the matrix to cache using lexical scoping
    set <- function(y)
      {
        x <<- y
        matrixCached <<- NULL
      }
  
    ## This function gets a matrix from cache
    getCachedmatrix <- function () matrixCached
  
    ## This function uses lexical scoping to store a matrix in cache
    setCachedmatrix <- function (y)
      {
          matrixCached <<- y
      }
    
    ## A list with the get and set functions is returned by this function  
      list(set = set, get= get, getCachedmatrix = getCachedmatrix, setCachedmatrix = setCachedmatrix)
}


## Function that retrieves cached matrix if it exists and inverses a matrix and calls  
## the set function in makeCacheMatrix to store it in cache if doesn't exist.   
## It checks if inverse of matrix exists in cache and only calles the set function
#  in makeCacheMatrix if it doesn't already exist.

cacheSolve <- function(x, ...) 
{
  ## Get matrix from cache
  matrixExisting <- x$getCachedmatrix()
    
  ## If matrix exists in cache, just return that one
   if (!is.null(matrixExisting))
    {
       message("Retrieving matrix from cache")
       return(matrixExisting)   
    } 
 
      
  ## Compute inverse of matrix if it doesn't exist already
  ## Call set function to store inverse of matrix in cache and return the inverted matrix
      message("Matrix not found in cache, computing inverse and storing it in cache")
      matrixNew <- x$get()
    ## Invert matrix
      matrixInverse <- solve(matrixNew)
    ## Store inverted matrix in cache
      matrixCached <-x$setCachedmatrix(matrixInverse)
    ## Return inverted matrix
        return(matrixCached)
}
