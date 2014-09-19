#JHU-  Programming Assignment 2: Lexical Scoping
# Description:This second programming assignment will require you to write an R function is able to cache potentially time-consuming computations.
#Matrix inversion is usually a costly computation and their may be some benefit to caching the inverse of a matrix 
# rather than compute it repeatedly. This Assignment is to write a pair of functions that cache the inverse of a matrix

# function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = numeric()) { 
  
  # initially set the value of cache to NULL if  nothing to cached
  cache <- NULL 
  
  # store the matrix 
  setMatrix <- function(newValue) 
  { 
    x <<- newValue 
    # clear/flush the cache since it is new value
    cache <<- NULL 
  } 
  
  # return the matrix 
  getMatrix <- function() { 
    x 
  } 
  
  
  # cache it!
  cacheInverse <- function(solve) { 
    cache <<- solve 
  } 
  
  
  # get the value  of the cached
  getInverse <- function() { 
    cache 
  } 
  
  # return a list
  list(setMatrix = setMatrix, getMatrix = getMatrix, cacheInverse = cacheInverse, getInverse = getInverse) 
} 



#This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) { 
  
  # get the value  of the cache
  
  inverse <- x$getInverse() 
  
  # if the value exists, return it 
  if(!is.null(inverse)) { 
    message("cached data") 
    return(inverse) 
  } 
  # otherwise get the matrix and caclulate the inverse and save to cache 
  data <- x$getMatrix() 
  inverse <- solve(data) 
  x$cacheInverse(inverse) 
  
  # return the inverse 
  inverse 
} 







