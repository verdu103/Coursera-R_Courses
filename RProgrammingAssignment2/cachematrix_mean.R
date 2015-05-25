## Put comments here that give an overall description of what your:
  # This is the second programming assignment in R for caching 
  # potentially time intensive computations. Some benefits from caching
  # include decreasing time by allowing the stored value to be looked
  # up versus repeated calculations.

## functions do list: makeCacheMatrix list of functions
  # set the value of the vector
  # get the value of the vector
  # set the value of the mean
  # get the value of the matrix 
  
## Write a short comment describing this function
  # the first function is creating a square matrix using makeCacheMatrix
  ##  solve can only work with square matrices

makeCacheMatrix <- function(x = numeric()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmean <- function(mean) m <<- mean
  getmean <- function() m
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean)
}

## Write a short comment describing this function
  # This second function calculates the mean of the special vector
  # created from makeVector.
  # This function checks for a mean first and if it does not see that 
  # it calculates the mean and stores it in cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getmean()
  if(!is.null(m)){
    message("Obtaining cached data...please hold")
    return(m)
  }
  data <- x$get()
  m <- mean(data, ...)
  x$setmean(m)
  m
}
x = rbind(c(1, -1/4), c(-1/4,1))
m = makeCacheMatrix(x)
m$get()
cacheSolve(m)
