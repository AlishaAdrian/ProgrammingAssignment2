## Alisha Adrian  Intro to R   Programming Assignment 2



## This R code contains functions that cache the inverse of a given matrix
## The given matrix is assumed to be invertable. Therefore, the code does
## not test for the determinate of the matrix or other conditions which
## are required for the matrix inverse to exit.



## This function takes a square, invertible matrix and returns a list of
## 4 functions to get/set the matrix and its inverse; as well as the matrix and
## matrix inverse are available in the parent calling environment

makeCacheMatrix <- function(mat = matrix()) {
 
  mat_inverse <- NULL   ## set mat_inverse to NULL
  
  ## function to set value of matrix to passed value and inverse to NuLL
  setmat <- function(y) {
    mat <<- y
    mat_inverse <<- NULL
  }
  
  ## function to get value of matrix
  getmat <- function() mat
  
  ## function to set value of mat_inverse
  setmat_inverse <- function(mi) mat_inverse <<- mi
  
  ## function to get value of mat_inverse
  getmat_inverse <- function() mat_inverse
  
  ## return list of functions explicitly and mat/mat_inverse via environment
  list(setmat = setmat, getmat = getmat,
       setmat_inverse = setmat_inverse,
       getmat_inverse = getmat_inverse)
  
# end function MakeCacheMatrix
}


## This function computes the inverse of a matrix if it has not already been
## calculated or if the matrix has changed. If the inverse has already been
## calculated, the inverse is retrieved from the cache instead.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  mat_inverse <- x$getmat_inverse() ## get matrix inverse from cache
  
## if matrix inverse is NOT NULL, it has been computed,
##   therefore get it from cache
  
  if(!is.null(mat_inverse)) {
    message("getting cached data")
    return(mat_inverse)
  }
  
## when matrix inverse has not been cached, compute it with solve()
  data <- x$getmat() ## get the matrix
  mat_inverse <- solve(data, ...) ## compute the inverse
  x$setmat_inverse(mat_inverse) ## store the inverse in cache
  mat_inverse  ## return the matrix inverse
}

## Example of using functions
## m1 = matrix(c(6,2,8,4), nrow=2, ncol=2, byrow=FALSE)
## y = makeCacheMatrix(m1)
## cacheSolve(y)


