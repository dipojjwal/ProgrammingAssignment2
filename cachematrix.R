## Put comments here that give an overall description of what your
## functions do
##The first function, makeCacheMatrix  creates a special "matrix",  a list containing  function to
##set the value of the matrix
##get the value of the matrix
##set the value of the inverse of matrix
##get the value of theinverse of matrix


makeCacheMatrix <- function(x = matrix()) {

m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinv <- function(solve) m <<- solve
    getinv <- function() m
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)

}


##This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed), 
##then the cachesolve should retrieve the inverse from the cache.
## Return a matrix that is the inverse of 'given matrix

cacheSolve <- function(x, ...) {
        
        
         m <- x$getinv()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinv(m)
    m
}
