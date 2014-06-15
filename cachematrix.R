## Cache the inverse of a matrix to minimize performance
## of repeated calls. 

## Create a "special" matrix which is actually a list 
## containing functions to set the value of the matrix,
## get the value of the matrix, set the inverse of the matrix, 
## and get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
   i <- NULL
   set <- function(y) {
      x <<- y
      i <<- NULL
   }
   get <- function() x
   set_inverse <- function(inverse) i <<- inverse
   get_inverse <- function() i
   list(set = set, get = get,
      set_inverse = set_inverse,
      get_inverse = get_inverse)
}

## Calculate the inverse of the "special" matrix. First
## check if the inverse has already been calculated. If so,
## use that value and skip recomputation. 

cacheSolve <- function(x, ...) {
   i <- x$get_inverse()
   if(!is.null(i)) {
      message("getting cached data")
      return(i)
   }
   data <- x$get()
   i <- solve(data, ...)
   x$set_inverse(i)
   i   
}
