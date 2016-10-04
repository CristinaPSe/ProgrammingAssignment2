## Put comments here that give an overall description of what your
## functions do

## function that creates a matrix that can cache its inverse

makeCacheMatrix <- function(x = matrix()) { #this is the matrix to be cached
  inv <- NULL #initialize inverse to null
  set <- function(y) {
    x <<- y #y will hold the value of x
    inv <<- NULL #initialize inverse to null inside the function
  }
  get <- function() {x} #returns the vector, x
  setInverse <- function(inverse) {inv <<- inverse} # sets the inv to inverse
  getInverse <- function() {inv} #returns the inverse
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse) #a vector with all the functions, 
                                #so they can be accesible outside the makeCacheMatrix
}


##function that calculates the inverse of the matrix created with makeCacheMatrix
##if theinverse has been calculated, return inverse from cache

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse() #get the inverse of the matrix defined inside x
  if (!is.null(inv)) { #if the inverse was already computed, return the cache version
    message("getting cached data")
    return(inv)
  }
  mat <- x$get() #get the underlying matrix
  inv <- solve(mat, ...) #calculate the inverse of the undelying vector
  x$setInverse(inv) #set the inverse in x and cache so we don't calculate again
  inv
}

