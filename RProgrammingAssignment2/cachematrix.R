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
  
## makeCacheMatrix: Write a short comment describing this function
  # the first function is creating a square matrix using makeCacheMatrix
  ##  solve can only work with square matrices

makeCacheMatrix <- function(x = numeric()) {
  my.inv <- NULL
  set <- function(y) {
    x <<- y
    my.inv <<- NULL
  }
  get <- function() x
  set.inv <- function(inverse) my.inv <<- inverse
  get.inv <- function() my.inv
  list(set = set, get = get,
       set.inv = set.inv,
       get.inv = get.inv)
}

## cacheSolve: Write a short comment describing this function
  # This second function calculates the inverse of the special vector
  # created from makeCacheMatrix.
  # This function checks for the inverse first and if it does not see
  # thatit calculates the inverse and stores it in cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  my.inv <- x$get.inv()
  if(!is.null(my.inv)){
    message("Obtaining cached data...please hold")
    return(my.inv)
  }
  data <- x$get()
  my.inv <- solve(data)
  x$set.inv(my.inv)
  my.inv
}

###Testing section: tests conducted using the functions above
# test one
x = rbind (c(1,-1/4), c(-1/4,1))
my.inv = makeCacheMatrix(x)
my.inv$get()
cacheSolve(my.inv)
##test one: first run results
  #        [,1]      [,2]
  # [1,] 1.0666667 0.2666667
  # [2,] 0.2666667 1.0666667

## test one: second run results
    #Obtaining cached data...please hold
  #         [,1]      [,2]
  # [1,] 1.0666667 0.2666667
  # [2,] 0.2666667 1.0666667

#test two
x = rbind (c(2, 1/2), c(1/2, 2))
my.inv = makeCacheMatrix(x)
myinv.get()
cacheSolve(my.inv)
##test two: first  run results
  #         [,1]       [,2]
  # [1,]  0.5333333 -0.1333333
  # [2,] -0.1333333  0.5333333
##test two: second run results
  #Obtaining cached data...please hold
  #         [,1]       [,2]
  # [1,]  0.5333333 -0.1333333
  # [2,] -0.1333333  0.5333333
