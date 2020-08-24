# The function, "makeCacheMatrix" creates a list 
# containing a function to

#1. set the value of the matrix
#2. get the value of the matrix
#3. set the value of the inverse
#4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL # m = inverse
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solvematrix) m <<- solvematrix
  getinverse <- function() m
  list(set = set, 
       get = get, 
       setinverse = setinverse, 
       getinverse = getinverse)
}

#The function "cacheSolve" calculates the inverse of the 
#matrix. 
#However, it first checks to see if the inverse has 
#already been calculated. If so, it gets the inverse 
#from the cache and skips the computation. Otherwise, 
#it calculates the inverse of the data and sets the value 
#of the inverse in the cache via the setmean function.

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setinverse(m)
  m      
}
