## The following functions create an object for a given matrix containing functions to cache the matrix and its inverse and to retrieve each of them. 
## When computing the inverse matrix, the cache is first checked to see if the computation has already occurred in the past and the cached value is returned, 
## if available. The inverse matrix is computed by calling the Solve function only if no cached value is available. This computed value is then cached and returned.


makeCacheMatrix <- function(x = matrix())
{ ## creates a special matrix object for the numeric matrix supplied as input
  
  i <- NULL	## Assigns local variable i (cache variable for the inverse of the supplied matrix) as NULL.
  
  set <- function(d)
  { ## takes a matrix as input
    
    x <<- d	## assigns the input to the matrix x in the containing environment (for caching the data)
    
    i <<- NULL	## resets the variable i in the containing environment (cached value of the inverse matrix) as NULL.
  }
  
  get <- function() x	## returns the cached original matrix data
  
  setInverse <- function(inverse) i <<- inverse	## takes a numeric matrix (for the inverse) as input and caches it in the global variable i.
  
  getInverse <- function() i	## returns the cached value of the inverse matrix.
  
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)	## a containing list for all the defined functions.
}

cacheSolve <- function(x, ...)
{ ## takes as input the special matrix object created by calling makeCacheMatrix()
  
  i <- x$getInverse()	## tries to retrieve a cached value of the inverse for the matrix object passed as input.
  
  if(!is.null(i))	## checks if a cached value of the inverse matrix is available.
  {
    message("Getting cached data:")	## message indicating that a cached value is being returned.
    return(i)	## cached inverse matrix is returned and execution stops
  }
  
  data <- x$get() ## if no cached inverse matrix is available, the cached original matrix is first retrieved
  
  i <- solve(data, ...)	## the inverse for the cached original matrix is computed
  
  x$setInverse(i)	## the computed inverse matrix is cached
  
  i	## the computed inverse matrix is returned
}