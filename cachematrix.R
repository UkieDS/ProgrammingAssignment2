## Matrix inversion is usually a costly computation so there are benefits  
## of caching the inverse of a matrix rather than compute it repeatedly 

## makeCacheMatrix - This function creates a special "matrix" object 
## that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) { ## Sub-function 1
        x <<- y
        m <<- NULL
    }
    get <- function() x ## Sub-function 2
    setmatrix <- function(solve) m <<- solve ## Sub-function 3
    getmatrix <- function() m ## Sub-function 4
    list(set = set, get = get, ## List all function
         setmatrix = setmatrix,
         getmatrix = getmatrix)
}


## cacheSolve - This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve will retrieve the inverse from the cache


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getmatrix()
    if(!is.null(m)) { ## Check if the inverse has already been calculated
        message("getting cached data") ## Print message and return m
        return(m)
    }
    matrix <- x$get()
    m <- solve(matrix, ...)
    x$setmatrix(m)
    m
}
