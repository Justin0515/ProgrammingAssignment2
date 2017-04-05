## makeCacheMatrix is a function that will return list containing 4 functions.
## makeCacheMatrix can cache the inverse of a matrix.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y){
                x <<- y
                inv <<- NULL
        }
        
        get <- function() {
                x
        }
        
        setInverse <- function(solveMatrix) {
                inv <<- solveMatrix
        }
        
        getInverse <- function() {
                inv
        }
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## cacheSolve gets the inverse of a matrix that is returned by makeCacheMatrix above.

cacheSolve <- function(x, ...) {
        inv <- x$getInverse()
        if(!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setInverse(inv)
        inv      
}